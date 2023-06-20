program prepbufr_encode_csv
!
! Write an entire CSV of observations into a prepbufr file
!
! Prepbufr CSV must be called prepbufr.csv and be located in the same directory
! as this script. The output BUFR file is simply called "prepbufr".
!
! Note: The organization of prepbufr files is still a little opaque to me. In
! prepbufr_decode_all.f90, there appear to be 3 tiers of organization: nmsg,
! ntb, and k. k is the easiest to interpret, it's simply the vertical level.
! I think ntb refers to the observation subset (e.g., a single radiosonde would
! have 2 ntbs, one for thermo variables and one for winds b/c these are
! separated as different types. Each of those ntbs would have several vertical
! levels, which is where k comes in). nmsg appear to be a little more random. 
! The prepbufr I was looking at had 4976 messages (nmsg), and I'm not sure how 
! they got that number. Maybe the encoders keeps filling a message until it
! reaches some maximum size? Only a few radiosonde obs are in a message, but a
! ton of mesonet obs can fit into one message.
!
! Luckily, how the prepbufr is structured does not matter here. That is taken
! care of in the Python script that creates the prepbufr CSV file.
!
! NOTE: This program CANNOT handle SIDs that have spaces in them (some MSONET 
! IDs have spaces). In order to avoid a runtime error when reading the CSV file
! (the error code will be 59), place all SIDs with spaces in single quotes ('
! ').
!
! shawn.s.murdzek@noaa.gov
! Date Created: 14 October 2022
!
 implicit none

 integer, parameter :: mxmn=65, mxlv=250
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ'
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE'
 character(80):: driftstr='XDR YDR HRDR'
 character(80):: sststr='MSST DBSS SST1 SSTQM SSTOE'
 character(80):: fcstr='TFC UFC VFC'
 character(80):: cldstr='VSSO CLAM HOCB'
 character(80):: maxminstr='MXTM MITM'
 character(80):: aircftstr='POAF IALR'
 character(80):: prwestr='PRWE'
 character(80):: prvstr='PRVSTG'
 character(80):: sprvstr='SPRVSTG'
 character(80):: howvstr='HOWV'
 character(80):: ceilstr='CEILING'
 character(80):: qifnstr='QIFN'
 character(80):: hblcsstr='HBLCS'
 character(80):: tsbstr='TSB'
 character(80):: acidstr='ACID'
 real(8) :: hdr(mxmn)
 real(8) :: temp(80)

 character(8) :: subset
 integer      :: unit_in=10,unit_out=20,unit_table=30
 integer      :: idate,iret,i,nmsg,ntb
 integer      :: tmsg,tntb
 real(8)      :: missing=1.0E11

 character(8) :: c_sid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)

 write(*,*) 'Starting CSV encoding program'
 
 open(unit_in,file='prepbufr.csv',action='read')
 open(unit_table,file='prepbufr.table',action='read')
 open(unit_out,file='prepbufr',action='write',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)

! Read first line in CSV file, which just contains headers
 read(unit_in,*)

! Loop over each line in CSV
 nmsg=0
 ntb=0
 do
   read(unit_in,*,end=100) tmsg,subset,idate,tntb,c_sid,(temp(i),i=1,68)

   if (nmsg /= tmsg) then
     write(*,*)
   endif

   write(*,*) tmsg,tntb,c_sid,' ',subset
       
   if (nmsg == tmsg) then
     ! Continue with same message
     if (ntb /= tntb) then
       ! Write data
       call writsb(unit_out)

       ! Start new ntb
       ntb=tntb
     
       hdr(1)=rstation_id
       do i=1,7
         hdr(i+1)=temp(i)
       enddo
       call ufbint(unit_out,hdr,mxmn,1,iret,hdstr)

     endif

   else
     ! Write data and close old message
     if (nmsg > 0) then
       call writsb(unit_out)
       call closmg(unit_out)
     endif

     ! Start new message
     nmsg=tmsg

     call openmb(unit_out,subset,idate)
     hdr(1)=rstation_id
     do i=1,7
       hdr(i+1)=temp(i)
     enddo
     call ufbint(unit_out,hdr,mxmn,1,iret,hdstr)
 
   endif
   
   if (sum(temp(8:20)).lt.(13*missing)) then 
     call ufbint(unit_out,temp(8:20),mxmn,1,iret,obstr)
   endif
   if (sum(temp(21:28)).lt.(8*missing)) then 
     call ufbint(unit_out,temp(21:28),mxmn,1,iret,qcstr)
   endif
   if (sum(temp(29:35)).lt.(7*missing)) then 
     call ufbint(unit_out,temp(29:35),mxmn,1,iret,oestr)
   endif
   if (sum(temp(36:38)).lt.(3*missing)) then 
     call ufbint(unit_out,temp(36:38),mxmn,1,iret,driftstr)
   endif
   if (sum(temp(39:43)).lt.(5*missing)) then 
     call ufbint(unit_out,temp(39:43),mxmn,1,iret,sststr)
   endif
   if (sum(temp(44:46)).lt.(3*missing)) then 
     call ufbint(unit_out,temp(44:46),mxmn,1,iret,fcstr)
   endif
   if (sum(temp(47:49)).lt.(3*missing)) then 
     call ufbint(unit_out,temp(47:49),mxmn,1,iret,cldstr)
   endif
   if (sum(temp(54:55)).lt.(2*missing)) then 
     call ufbint(unit_out,temp(54:55),mxmn,1,iret,maxminstr)
   endif
   if (sum(temp(56:57)).lt.(2*missing)) then 
     call ufbint(unit_out,temp(56:57),mxmn,1,iret,aircftstr)
   endif
   if (temp(58).lt.missing) then 
     call ufbint(unit_out,temp(58),mxmn,1,iret,prwestr)
   endif
   if (temp(59).lt.missing) then 
     call ufbint(unit_out,temp(59),mxmn,1,iret,prvstr)
   endif
   if (temp(60).lt.missing) then 
     call ufbint(unit_out,temp(60),mxmn,1,iret,sprvstr)
   endif
   if (temp(61).lt.missing) then 
     call ufbint(unit_out,temp(61),mxmn,1,iret,howvstr)
   endif
   if (temp(62).lt.missing) then 
     call ufbint(unit_out,temp(62),mxmn,1,iret,ceilstr)
   endif
   if (temp(63).lt.missing) then 
     call ufbint(unit_out,temp(63),mxmn,1,iret,qifnstr)
   endif
   if (temp(64).lt.missing) then 
     call ufbint(unit_out,temp(64),mxmn,1,iret,hblcsstr)
   endif
   if (temp(65).lt.missing) then 
     call ufbint(unit_out,temp(65),mxmn,1,iret,tsbstr)
   endif
   if (temp(66).lt.missing) then 
     call ufbint(unit_out,temp(66),mxmn,1,iret,acidstr)
   endif

 enddo

100 close(unit_in)
 call writsb(unit_out)
 call closmg(unit_out)
 call closbf(unit_out)
 write(*,*)
 write(*,*) 'Program finished successfully'

end program

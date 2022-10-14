program prepbufr_encode_csv
!
! Write an entire CSV of observations into a prepbufr file
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
! shawn.s.murdzek@noaa.gov
! Date Created: 14 October 2022
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)
 real(8) :: temp(mxmn)

 character(8) :: subset,tsubset
 integer      :: unit_in=10,unit_out=20,unit_table=30
 integer      :: idate,iret,nlvl,i,k,nmsg,ntb
 integer      :: tmsg,tdate,tntb

 character(8) :: c_sid,tsid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)
!
! write observations into prepbufr file
!
 open(unit_in,file='prepbufr.csv',action='read')
 open(unit_table,file='prepbufr.table',action='read')
 open(unit_out,file='prepbufr',action='write',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)

! Read first line in CSV file, which just contains headers
 read(unit_in,'(1x 35(a0,","))')

! Loop over each line in CSV
 nmsg=0
 ntb=0
 k=1
 do
   read(unit_in,'(1x 35(g0,","))',end=100) tmsg,tsubset,tdate,tntb,tsid,(temp(i),i=1,30)

   if (nmsg == tmsg) then
     ! Continue with same message
     if (ntb /= tntb) then
       ! Write data
       call ufbint(unit_out,hdr,mxmn,1,iret,hdstr)
       call ufbint(unit_out,obs,mxmn,k,iret,obstr)
       call ufbint(unit_out,oer,mxmn,k,iret,oestr)
       call ufbint(unit_out,qcf,mxmn,k,iret,qcstr)
       call writsb(unit_out)

       ! Start new ntb
       ntb=tntb
       k=1

     endif

   else
     ! Write data and close old message
     call ufbint(unit_out,hdr,mxmn,1,iret,hdstr)
     call ufbint(unit_out,obs,mxmn,k,iret,obstr)
     call ufbint(unit_out,oer,mxmn,k,iret,oestr)
     call ufbint(unit_out,qcf,mxmn,k,iret,qcstr)
     call writsb(unit_out)
     call closmg(unit_out)

     ! Start new message
     nmsg=tmsg
     k=1

     call openmb(unit_out,subset,idate)
     hdr(1)=rstation_id
     do i=1,7
       hdr(i+1)=temp(i)
     enddo
 
   endif

   do i=1,9
     obs(i,k)=temp(i+7)
   enddo
   do i=1,7
     qcf(i,k)=temp(i+16)
   enddo
   do i=1,7
     oer(i,k)=temp(i+23)
   enddo

   k=k+1

 enddo

100 close(unit_in)
 call closbf(unit_out)

end program

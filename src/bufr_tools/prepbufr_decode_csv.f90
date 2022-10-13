program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
! write all obs to a CSV that can be easily read by Python
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
 integer, parameter :: nhd=8, nob=9, nqc=7, noe=7

! Define fields to be read from prepbufr file
 character(4), dimension(nhd) :: hda=(/ 'SID','XOB','YOB','DHR','TYP','ELV','SAID','T29' /)
 character(4), dimension(nob) :: oba=(/ 'POB','QOB','TOB','ZOB','UOB','VOB','PWO','CAT','PRSS' /)
 character(4), dimension(nqc) :: qca=(/ 'PQM','QQM','TQM','ZQM','WQM','NUL','PWQ' /)
 character(4), dimension(noe) :: oea=(/ 'POE','QOE','TOE','NUL','WOE','NUL','PWE' /)
 character(80) :: hdstr,obstr,qcstr,oestr

 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb,nobs

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret

! Create strings to read prepbufr fields
 write(hdstr,'(*(a," "))') hda
 write(obstr,'(*(a," "))') oba
 write(qcstr,'(*(a," "))') qca
 write(oestr,'(*(a," "))') oea

! Open files
 open(24,file='prepbufr.table')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
 open(100,file='prepbufr.csv')

! nmsg = Message number
! ntb = Observation number in this particular message
 write(100,'(1x *(g0,","))') 'nmsg','subset','cycletime','ntb',(trim(hda(i)),i=1,nhd), &
                             (trim(oba(i)),i=1,nob),(trim(qca(i)),i=1,nqc),            &
                             (trim(oea(i)),i=1,noe)

 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)

 nmsg=0
 msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
   nmsg=nmsg+1
   ntb = 0
   sb_report: do while (ireadsb(unit_in) == 0)
     ntb = ntb+1
     call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
     call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
     call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
     call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
     rstation_id=hdr(1)
     do k=1,iret
       write(100, '(1x *(g0,","))') nmsg,trim(subset),idate,ntb,trim(c_sid),(hdr(i),i=2,nhd), &
                                    (obs(i,k),i=1,nob),(qcf(i,k),i=1,nqc),(oer(i,k),i=1,noe)
     enddo
   enddo sb_report
 enddo msg_report
 call closbf(unit_in)

end program

program prepbufr_decode_csv
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
! write all obs to a CSV that can be easily read by Python
!
! shawn.s.murdzek@noaa.gov
! Date Created: 14 October 2022
!
 implicit none

 integer, parameter :: mxmn=65, mxlv=250
 integer, parameter :: nhd=8, nob=13, nqc=8, noe=7, ndrift=3, nsst=5, nmisc1=7, nmisc2=5

! Define fields to be read from prepbufr file
! The following fields cause an error in the BUFR library: VSSO, CLAM, HOCB, PRWE, MXTM, MITM
! Error: INPUT STRING STORE NODES (MNEMONICS) THAT ARE IN MORE THAN ONE REPLICATION GROUP
 character(4), dimension(nhd) :: hda=(/ 'SID','XOB','YOB','DHR','TYP','ELV','SAID','T29' /)
 character(4), dimension(nob) :: oba=(/ 'POB','QOB','TOB','ZOB','UOB','VOB','PWO','MXGS','HOVI','CAT','PRSS','TDO','PMO' /)
 character(4), dimension(nqc) :: qca=(/ 'PQM','QQM','TQM','ZQM','WQM','NUL','PWQ','PMQ' /)
 character(4), dimension(noe) :: oea=(/ 'POE','QOE','TOE','NUL','WOE','NUL','PWE' /)
 character(4), dimension(ndrift) :: drifta=(/ 'XDR','YDR','HRDR' /)
 character(5), dimension(nsst) :: ssta=(/ 'MSST','DBSS','SST1','SSTQM','SSTOE' /)
 character(7), dimension(nmisc1) :: misc1a=(/ 'PRVSTG','SPRVSTG','CDTP','GCDTT','CDTP_QM','HOWV','CEILING' /)
 character(4), dimension(nmisc2) :: misc2a=(/ 'QIFN','TOCC','HBLCS','POAF','IALR' /)
 character(80) :: hdstr,obstr,qcstr,oestr,driftstr,sststr,miscstr1,miscstr2

 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv),drift(mxmn,mxlv),sst(mxmn,mxlv), &
            misc1(mxmn,mxlv),misc2(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb,nobs

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret_max
 integer, dimension(8) :: iret

 print*, 'starting prepbufr_decode_csv program'

! Create strings to read prepbufr fields
 write(hdstr,'(*(a," "))') hda
 write(obstr,'(*(a," "))') oba
 write(qcstr,'(*(a," "))') qca
 write(oestr,'(*(a," "))') oea
 write(driftstr,'(*(a," "))') drifta
 write(sststr,'(*(a," "))') ssta
 write(miscstr1,'(*(a," "))') misc1a
 write(miscstr2,'(*(a," "))') misc2a

! Open files
 open(24,file='prepbufr.table')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
 open(100,file='prepbufr.csv')

! nmsg = Message number
! ntb = Observation number in this particular message
 write(100,'(1x *(g0,","))') 'nmsg','subset','cycletime','ntb',(trim(hda(i)),i=1,nhd), &
                             (trim(oba(i)),i=1,nob),(trim(qca(i)),i=1,nqc),            &
                             (trim(oea(i)),i=1,noe),(trim(drifta(i)),i=1,ndrift),      &
                             (trim(ssta(i)),i=1,nsst),(trim(misc1a(i)),i=1,nmisc1),    &
                             (trim(misc2a(i)),i=1,nmisc2)

 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)

 nmsg=0
 msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
   nmsg=nmsg+1
   ntb = 0
   sb_report: do while (ireadsb(unit_in) == 0)
     ntb = ntb+1
     iret = 0
     ! Read header last so that iret contains all entries 
     call ufbint(unit_in,hdr,mxmn,1   ,iret(1),hdstr)
     call ufbint(unit_in,oer,mxmn,mxlv,iret(2),oestr)
     call ufbint(unit_in,qcf,mxmn,mxlv,iret(3),qcstr)
     call ufbint(unit_in,obs,mxmn,mxlv,iret(4),obstr)
     call ufbint(unit_in,drift,mxmn,mxlv,iret(5),driftstr)
     call ufbint(unit_in,sst,mxmn,mxlv,iret(6),sststr)
     call ufbint(unit_in,misc1,mxmn,mxlv,iret(7),miscstr1)
     call ufbint(unit_in,misc2,mxmn,mxlv,iret(8),miscstr2)
     rstation_id=hdr(1)
     iret_max = maxval(iret)
     do k=1,iret_max
       write(100, '(1x *(g0,","))') nmsg,trim(subset),idate,ntb,trim(c_sid),(hdr(i),i=2,nhd), &
                                    (obs(i,k),i=1,nob),(qcf(i,k),i=1,nqc),(oer(i,k),i=1,noe), &
                                    (drift(i,k),i=1,ndrift),(sst(i,k),i=1,nsst),              &
                                    (misc1(i,k),i=1,nmisc1),(misc2(i,k),i=1,nmisc2)
     enddo
   enddo sb_report
 enddo msg_report
 call closbf(unit_in)

end program

program  GL_report
             implicit none

             integer i,j,k,n
             real A(2),B(2),sumA(2),sumB(2),aveA(2) ,aveB(2)!constant
             real bA(2), bB(2), eA(2), eB(2),zbA,zbB !vailidate
             real time, eps
             sumA=0
             sumB=0
             time=0
             eps=10
             data bA/0,0/
             bB=bA
             k=200

             do i=1,30
               do j=1,6
                 read(5,*)A(1),A(2),B(1),B(2)
                 sumA=sumA+A
                 sumB=sumB+B
                 time=time+1
               enddo
             enddo
             aveA=sumA/time
             aveB=sumB/time


             write(6,*)bA,bB
             do n=0,k
             zbA=1+exp(bA(1))+exp(bA(2))+exp(bA(1)+bA(2))!A
             zbB=1+exp(bB(1))+exp(bB(2))+exp(bB(1)+bB(2))!B
             eA(1)=(exp(bA(1))+exp(bA(1)+bA(2)))/zbA
             eA(2)=(exp(bA(2))+exp(bA(1)+bA(2)))/zbA
             eB(1)=(exp(bB(1))+exp(bB(1)+bB(2)))/zbB
             eB(2)=(exp(bB(2))+exp(bB(1)+bB(2)))/zbB
             bA=bA-eps*(eA-aveA)
             bB=bB-eps*(eB-aveB)
             write(6,*)bA,bB
             enddo

             write(6,*) eA,eB

     end program

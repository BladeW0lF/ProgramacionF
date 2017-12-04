program summation
implicit none
integer :: sum, a, count
real :: arit, harm, sumainv
real :: fa, fc, fs

print*, "Este programa realiza las medias de una sumatoria,"
print*, "cuando quiera aplaste 0 para terminar"
open(unit=10, file="SumData.DAT", status='unknown')

suma = 0
count = 0
sumainv = 0

do
 print*, "Add:"
 read*, a
 if (a == 0) then
  exit
 else
sum = sum + a
count = count + 1
fa = float(a)
fa = 1/fa
sumainv = sumainv + fa

 end if
 write(10,*) a
end do
fs = float(sum)
fc = float(count)
arit = fs / fc
harm = fc / sumainv


print*, "Sumatoria =", sum
write(10,*) "Sumatoria =", sum
write(10,*)' '
print*, "Media aritmetica =", arit
write(10,*) "Media aritmetica =", arit
write(10,*) ' '
print*, "Media armonica =", harm
write(10,*) "Media armonica =", harm
write(10,*) ' '


close(10)

end

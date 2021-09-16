# Matlab_Study
Using matlab for evaluating EMG and IMU signal
--------
# code 정리

`TF1 = islocalmin(filtsig, 'FlatSelection', 'last');`

연속적으로 나타나는 국소 최솟값을 표시하는 방법을 지정->연속되는 최솟값중 마지막 부분 지정=flat한 신호에서 마지막 toe off 신호 잡기


`heelstrikeidx = rmmissing(init_idx);`

heelstriedidx에서 init_idx 요소를 제거

`width(T)`

returns the number of variables in table T

`r = rem(a,b)`

r = rem(a,b)는 a를 b로 나눈 후 나머지를 반환합니다.

`subplot(m,n,p)`

현재 Figure를 m×n 그리드로 나누고, p로 지정된 위치에 좌표축을 만듭니다

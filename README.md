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

`[피크 _ 값, 피크 _ 위치] = findpeaks (my_signal, 'minpeakheight', 2.5)`

"minpeakheight"매개 변수를 사용하여 최소 높이의 피크를 검색합니다. 높이는 허용 가능한 피크의 최소 데이터 값을 나타내는 실수 값 스칼라입니다.

`[피크 _ 값, 피크 _ 위치] = findpeaks (my_signal, 'minpeakdistance', 5);`

"minpeakdistance"매개 변수를 사용하여 최소 거리로 분리 된 피크를 검색합니다. 값은 "my_signal"벡터에서 피크 사이의 최소 인덱스 수이며 정수여야합니다.

`F = griddedInterpolant(x,v)`

샘플 점 x와 해당 값 v로 이루어진 벡터에서 1차원 보간을 생성합니다

`plot(signal_IMU_time(:,1), signal_IMU(:,imunum), 'k-', 'linew', 1.5)`

=라인은 검은색 linew=linewidth로 라인 굵기는 1.5로 plot

`plot(signal_IMU_time(maxpeak,1), signal_IMU(maxpeak,imunum),'gp','linew',1,'markersize',2,'markerfacecolor','b')`

=gp는 마커포인트에 별모양표시, 마커색은 파란색

`clearvars variables`

변수가 전역적인 경우 clearvars는 현재 작업 공간에서만 해당 변수를 제거할 뿐, 그 변수를 전역 변수로 선언한 함수에서는 계속해서 액세스할 수 있도록 변수를 유지합니다.

` fs = length(signal_IMU)/signal_IMU_time(end,1)`

fs는 기울기=signal_IMU 요소의 개수/signal_IMU_time의 마지막항 1열(=마지막시간)

`IMUterm(1,:) = 1:18:min(size(signal_IMU))`

IMUterm의 1행 모든열을 1부터 min(size(signal_IMU))까지 18간격으로 나열

`M = mean(A)`

크기가 1이 아닌 첫 번째 배열 차원에서 A의 요소의 평균값을 반환합니다.

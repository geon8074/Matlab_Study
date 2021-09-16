clear; close all; 
subjectNum = 'SP_01';
experimentOrder = '1';
subjectInfo = strcat(subjectNum,'_',experimentOrder);
% path_name = strcat(C:/Users/Home/Desktop/Ansan/splitFile/',subjectInfo);
path_name = strcat('C:\Users\Home\Desktop\Ansan\splitFile\',subjectInfo);
load(strcat(path_name,'.mat'))

%signal length [IMU = FSR]
speed = 1  %=1:protocol_num

% FSR (only heel data included)
if speed ==1
    raw_10MWT_L = FSR_SP_01_10MWT_n_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_n_02(:,3:4);
elseif speed ==2
    raw_10MWT_L = FSR_SP_01_10MWT_s_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_s_02(:,3:4);
elseif speed ==3
    raw_10MWT_L = FSR_SP_01_10MWT_f_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_f_02(:,3:4);
end

tic
for foot = 1:2  % 1: Left foot, 2: Right foot
    if foot==1
        FSR_signal = raw_10MWT_L;
    elseif foot==2
        FSR_signal = raw_10MWT_R;
    end
    
    % 그래프로 확인하기 x축 시간, y축 FSR신호
    plot(FSR_signal(1:end,1),FSR_signal(1:end,2)); hold on
    
    % interpolation 해서 0으로 끊긴 데이터들에 대해서 보정하기
    srate = length(FSR_signal)/FSR_signal(end,1);   % FSR sampling rate
    
    [~,maxpeak] = findpeaks(FSR_signal(:,2), 'MinPeakHeight',40,'MinPeakDistance',srate*0.5); % 50deg/sec 이상, 0.5초 간격 이상
    plot(FSR_signal(maxpeak,1), FSR_signal(maxpeak,2),'gp','linew',1,'markersize',7,'markerfacecolor','b')
    
    temp = FSR_signal(1:maxpeak(end),2) == 0;  % maxpeak 길이 까지만 신호 끊기는 구간 detection
    temp(maxpeak(end):length(FSR_signal)) = zeros; % size 맞추기
    outliers = temp;  % temp를 outlier로 변경
    
    F = griddedInterpolant(FSR_signal(~outliers,1), FSR_signal(~outliers,2));
    signalRR = FSR_signal(:,2);
    signalRR(outliers) = F(FSR_signal(outliers,1));
    FSR_signal(:,2)=signalRR;
    plot(FSR_signal(:,1), FSR_signal(:,2), 'k-', 'linew', 1.5)
    legend('Raw signal','Interpolated')

    
    
    if speed==1
        prot_name='n_02';
    elseif speed==2
        prot_name='s_02';
    elseif speed==3
        prot_name='f_02';
    end
    
    
    % sensor_signal ==> IMU and EMG data
    eval([ 'sensor_signal =' subjectNum '_10MWT_' prot_name ';' ]);
    
    signal_IMU = sensor_signal(:, [4	6	8	10	12	14	16	18	20	24	26	28	30	32	34	36	38	40	44	46	48	50	52	54	56	58	60	64	66	68	70	72	74	76	78	80	84	86	88	90	92	94	96	98	100	104	106	108	110	112	114	116	118	120	124	126	128	130	132	134	136	138	140	144	146	148	150	152	154	156	158	160	164	166	168	170	172	174	176	178	180	184	186	188	190	192	194	196	198	200	204	206	208	210	212	214	216	218	220	224	226	228	230	232	234	236	238	240]);
    signal_IMU(length(FSR_signal)+1:end,:) = [];
    signal_IMU_time(:,1) = sensor_signal(:,3);
    signal_IMU_time(length(FSR_signal)+1:end,:) = [];
    
    signal_EMG = sensor_signal(:,[2	22	42	62	82	102	122	142	162	182	202	222]);
    signal_EMG(find(sensor_signal(:,1)==FSR_signal(end,1))+1:end,:) = [];  % NaN 데이터 삭제
    signal_EMG(:, [7 8 9 10]) = []; % remain 3 shank, 1 thigh sensor data
    signal_EMG_time(:,1) = sensor_signal(1:length(signal_EMG),1); % Nan data 삭제 (signal_EMG와 크기 일치)

    
    %% median filter
    
    % visual-picked threshold
    threshold = -1;
    
    % find data values above the threshold
    suprathresh = find( FSR_signal(:,2)>threshold );
    
    % initialize filtered signal
    filtsig = FSR_signal(:,2);
    
    % loop through suprathreshold points and set to median of k
    k = 20; % actual window is k*2+1
    for ti=1:length(suprathresh)
        
        % find lower and upper bounds
        lowbnd = max(1,suprathresh(ti)-k);
        uppbnd = min(suprathresh(ti)+k,length(FSR_signal));
        
        % compute median of surrounding points
        filtsig(suprathresh(ti)) = median(FSR_signal(lowbnd:uppbnd,2));
    end
    
    % flat selection
    TF1 = islocalmin(filtsig, 'FlatSelection', 'last');
    
    init_idx = zeros;
    t = 1; 
    for lenTF = 1 : length(TF1)
        if TF1(lenTF) == 1 
           init_idx(t,1) = lenTF;    % heel strike index before refining 
           init_idx(t,2) = FSR_signal(lenTF,1); 
           t = t+1;
        end
    end
    
    % threshold 보다 높은 값들 nan으로 지정
    for heelpoint = 1 : length(init_idx)
        if filtsig(init_idx(heelpoint,1)) >= 5 % threshold
            init_idx(heelpoint,:) = nan;
        end
    end
    heelstrikeidx = rmmissing(init_idx); % remove Nan elements
  
    if foot==1
        FSR_signal = raw_10MWT_L;
    elseif foot==2
        FSR_signal = raw_10MWT_R;
    end
    
    % 그래프로 확인하기 x축 시간, y축 FSR신호
    plot(FSR_signal(1:end,1),FSR_signal(1:end,2)); hold on
    
    % interpolation 해서 0으로 끊긴 데이터들에 대해서 보정하기
    srate = length(FSR_signal)/FSR_signal(end,1);   % FSR sampling rate
    
    [~,maxpeak] = findpeaks(FSR_signal(:,2), 'MinPeakHeight',40,'MinPeakDistance',srate*0.5); % 50deg/sec 이상, 0.5초 간격 이상
    plot(FSR_signal(maxpeak,1), FSR_signal(maxpeak,2),'gp','linew',1,'markersize',7,'markerfacecolor','b')
    
    temp = FSR_signal(1:maxpeak(end),2) == 0;  % maxpeak 길이 까지만 신호 끊기는 구간 detection
    temp(maxpeak(end):length(FSR_signal)) = zeros; % size 맞추기
    outliers = temp;  % temp를 outlier로 변경
    
    F = griddedInterpolant(FSR_signal(~outliers,1), FSR_signal(~outliers,2));
    signalRR = FSR_signal(:,2);
    signalRR(outliers) = F(FSR_signal(outliers,1));
    FSR_signal(:,2)=signalRR;
    plot(FSR_signal(:,1), FSR_signal(:,2), 'k-', 'linew', 1.5)
    legend('Raw signal','Interpolated')

    
    
    if speed==1
        prot_name='n_02';
    elseif speed==2
        prot_name='s_02';
    elseif speed==3
        prot_name='f_02';
    end
    
    
    % sensor_signal ==> IMU and EMG data
    eval([ 'sensor_signal =' subjectNum '_10MWT_' prot_name ';' ]);
    
    signal_IMU = sensor_signal(:, [4	6	8	10	12	14	16	18	20	24	26	28	30	32	34	36	38	40	44	46	48	50	52	54	56	58	60	64	66	68	70	72	74	76	78	80	84	86	88	90	92	94	96	98	100	104	106	108	110	112	114	116	118	120	124	126	128	130	132	134	136	138	140	144	146	148	150	152	154	156	158	160	164	166	168	170	172	174	176	178	180	184	186	188	190	192	194	196	198	200	204	206	208	210	212	214	216	218	220	224	226	228	230	232	234	236	238	240]);
    signal_IMU(length(FSR_signal)+1:end,:) = [];
    signal_IMU_time(:,1) = sensor_signal(:,3);
    signal_IMU_time(length(FSR_signal)+1:end,:) = [];
    
    signal_EMG = sensor_signal(:,[2	22	42	62	82	102	122	142	162	182	202	222]);
    signal_EMG(find(sensor_signal(:,1)==FSR_signal(end,1))+1:end,:) = [];  % NaN 데이터 삭제
    signal_EMG(:, [7 8 9 10]) = []; % remain 3 shank, 1 thigh sensor data
    signal_EMG_time(:,1) = sensor_signal(1:length(signal_EMG),1); % Nan data 삭제 (signal_EMG와 크기 일치)

    
    %% median filter
    
    % visual-picked threshold
    threshold = -1;
    
    % find data values above the threshold
    suprathresh = find( FSR_signal(:,2)>threshold );
    
    % initialize filtered signal
    filtsig = FSR_signal(:,2);
    
    % loop through suprathreshold points and set to median of k
    k = 20; % actual window is k*2+1
    for ti=1:length(suprathresh)
        
        % find lower and upper bounds
        lowbnd = max(1,suprathresh(ti)-k);
        uppbnd = min(suprathresh(ti)+k,length(FSR_signal));
        
        % compute median of surrounding points
        filtsig(suprathresh(ti)) = median(FSR_signal(lowbnd:uppbnd,2));
    end
    
    % flat selection
    TF1 = islocalmin(filtsig, 'FlatSelection', 'last');
    
    init_idx = zeros;
    t = 1; 
    for lenTF = 1 : length(TF1)
        if TF1(lenTF) == 1 
           init_idx(t,1) = lenTF;    % heel strike index before refining 
           init_idx(t,2) = FSR_signal(lenTF,1); 
           t = t+1;
        end
    end
    
    % threshold 보다 높은 값들 nan으로 지정
    for heelpoint = 1 : length(init_idx)
        if filtsig(init_idx(heelpoint,1)) >= 5 % threshold
            init_idx(heelpoint,:) = nan;
        end
    end
    heelstrikeidx = rmmissing(init_idx); % remove Nan elements
    %% IMU segmentation
    % IMU data interpolation 해서 0 구간 보정
    fignum = 1;
    for imunum = 1 : width(signal_IMU)
        if rem(imunum, 9) == 1
         figure;   
             fignum = 1;
        end
        
        subplot(3,3,fignum)        
        plot(signal_IMU_time(:,1), signal_IMU(:,imunum)); hold on
        
        [~,maxpeak] = findpeaks(signal_IMU(:,imunum), 'MinPeakDistance',srate*0.5); % 50deg/sec 이상, 0.5초 간격 이상
        temp = signal_IMU(1:maxpeak(end),imunum) == 0;  % maxpeak 길이 까지만 신호 끊기는 구간 detection
        temp(maxpeak(end):length(signal_IMU)) = zeros; % size 맞추기
        outliers = temp;  % temp를 outlier로 변경
        
        F = griddedInterpolant(signal_IMU_time(~outliers,1), signal_IMU(~outliers,imunum));
        signalRR = signal_IMU(:,imunum);
        signalRR(outliers) = F(signal_IMU_time(outliers,1));
        signal_IMU(:,imunum)=signalRR;
        
        plot(signal_IMU_time(:,1), signal_IMU(:,imunum), 'k-', 'linew', 1.5)
        plot(signal_IMU_time(maxpeak,1), signal_IMU(maxpeak,imunum),'gp','linew',1,'markersize',2,'markerfacecolor','b')
        
        fignum = fignum+1;
        clearvars maxpeak
    end
    legend('Raw signal','Interpolated')
    
    
    fs = length(signal_IMU)/signal_IMU_time(end,1);
    
    IMUterm(1,:) = 1:18:min(size(signal_IMU));
    IMUterm(2,:) = 10:18:min(size(signal_IMU));
    
    if foot ==1
        % 1st~5th column : EMG signal, 6th column : time segment
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 1, 3, 5, 7번 센서
            for j = 1 : 4
                segIMUL{i,j} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(1,j):IMUterm(1,j)+8);
            end
        end
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 11번 센서
            segIMUL{i,5} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(1,6):IMUterm(1,6)+8);
        end
        
        % time segmentation --> last column 추가
        temp_size=min(size(segIMUL))+1;
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 4 ch
            segIMUL{i,temp_size} = signal_IMU_time(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1);
        end
        
    elseif foot ==2
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 2, 4, 6, 8번 센서
            for j = 1 : 4
                segIMUR{i,j} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(2,j):IMUterm(2,j)+8);
            end
        end
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 12번 센서
            segIMUR{i,5} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(2,6):IMUterm(2,6)+8);
        end
        
        % time segmentation --> last column 추가
        temp_size=min(size(segIMUR))+1;
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 4 ch
            segIMUR{i,temp_size} = signal_IMU_time(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1);
        end
    end

    clear signal_IMU_time

    % EMG segmentation
    % EMG offset 제거
    for i = 1 : 8
        signal_EMG(:,i) = signal_EMG(:,i) - mean(signal_EMG(:,i));
    end

    % EMG gait cycle time index
    try
        t1=1; t2=1;
        for  i = 1 :length(signal_EMG_time)
            if round(signal_EMG_time(i),3) == round(heelstrikeidx(t2,2),3)
                heelstrikeidx_EMG(t1,1) = signal_EMG_time(i);
                heelstrikeidx_EMG(t1,2) = i;
                t1=t1+1;
                t2=t2+1;
            end
        end
    catch
    end
    
    % EMG gait cycle segmentation
    if foot==1
        for i = 1 : length(heelstrikeidx_EMG)-1  % cycle 별 EMG 신호 나눔 (Left)
            for j = 1:4
                segEMGL{i,j} = signal_EMG(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1,j);
            end
            segEMGL{i,5} = signal_EMG_time(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1);        % time segmentation --> last column 추가
        end
                % time segmentation --> last column 추가
    elseif foot ==2
        for i = 1 : length(heelstrikeidx_EMG)-1  % cycle 별 EMG 신호 나눔 (right)
            for j = 1:4
                segEMGR{i,j} = signal_EMG(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1,j);
            end
            segEMGR{i,5} = signal_EMG_time(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1);        % time segmentation --> last column 추가
        end
    end
    
    
end
toc

speed =

     1

경과 시간은 7.254384초입니다.
clear; close all; 
subjectNum = 'SP_01';
experimentOrder = '1';
subjectInfo = strcat(subjectNum,'_',experimentOrder);
% path_name = strcat('/Volumes/T7/백업파일/USB/고대안산병원실험데이터/Database/DB_02/splitFile/',subjectInfo);
path_name = strcat('C:\Users\Home\Desktop\Ansan\splitFile\',subjectInfo);
load(strcat(path_name,'.mat'))

%signal length [IMU = FSR]
speed = 1  %=1:protocol_num

% FSR (only heel data included)
if speed ==1
    raw_10MWT_L = FSR_SP_01_10MWT_n_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_n_02(:,3:4);
elseif speed ==2
    raw_10MWT_L = FSR_SP_01_10MWT_s_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_s_02(:,3:4);
elseif speed ==3
    raw_10MWT_L = FSR_SP_01_10MWT_f_02(:,1:2);
    raw_10MWT_R = FSR_SP_01_10MWT_f_02(:,3:4);
end

tic
for foot = 1:2  % 1: Left foot, 2: Right foot
    if foot==1
        FSR_signal = raw_10MWT_L;
    elseif foot==2
        FSR_signal = raw_10MWT_R;
    end
    
    % 그래프로 확인하기
    plot(FSR_signal(1:end,1),FSR_signal(1:end,2)); hold on
    
    % interpolation 해서 0으로 끊긴 데이터들에 대해서 보정하기
    srate = length(FSR_signal)/FSR_signal(end,1);   % FSR sampling rate
    
    [~,maxpeak] = findpeaks(FSR_signal(:,2), 'MinPeakHeight',40,'MinPeakDistance',srate*0.5); % 50deg/sec 이상, 0.5초 간격 이상
    plot(FSR_signal(maxpeak,1), FSR_signal(maxpeak,2),'gp','linew',1,'markersize',7,'markerfacecolor','b')
    
    temp = FSR_signal(1:maxpeak(end),2) == 0;  % maxpeak 길이 까지만 신호 끊기는 구간 detection
    temp(maxpeak(end):length(FSR_signal)) = zeros; % size 맞추기
    outliers = temp;  % temp를 outlier로 변경
    
    F = griddedInterpolant(FSR_signal(~outliers,1), FSR_signal(~outliers,2));
    signalRR = FSR_signal(:,2);
    signalRR(outliers) = F(FSR_signal(outliers,1));
    FSR_signal(:,2)=signalRR;
    plot(FSR_signal(:,1), FSR_signal(:,2), 'k-', 'linew', 1.5)
    legend('Raw signal','Interpolated')

    
    
    if speed==1
        prot_name='n_02';
    elseif speed==2
        prot_name='s_02';
    elseif speed==3
        prot_name='f_02';
    end
    
    
    % sensor_signal ==> IMU and EMG data
    eval([ 'sensor_signal =' subjectNum '_10MWT_' prot_name ';' ]);
    
    signal_IMU = sensor_signal(:, [4	6	8	10	12	14	16	18	20	24	26	28	30	32	34	36	38	40	44	46	48	50	52	54	56	58	60	64	66	68	70	72	74	76	78	80	84	86	88	90	92	94	96	98	100	104	106	108	110	112	114	116	118	120	124	126	128	130	132	134	136	138	140	144	146	148	150	152	154	156	158	160	164	166	168	170	172	174	176	178	180	184	186	188	190	192	194	196	198	200	204	206	208	210	212	214	216	218	220	224	226	228	230	232	234	236	238	240]);
    signal_IMU(length(FSR_signal)+1:end,:) = [];
    signal_IMU_time(:,1) = sensor_signal(:,3);
    signal_IMU_time(length(FSR_signal)+1:end,:) = [];
    
    signal_EMG = sensor_signal(:,[2	22	42	62	82	102	122	142	162	182	202	222]);
    signal_EMG(find(sensor_signal(:,1)==FSR_signal(end,1))+1:end,:) = [];  % NaN 데이터 삭제
    signal_EMG(:, [7 8 9 10]) = []; % remain 3 shank, 1 thigh sensor data
    signal_EMG_time(:,1) = sensor_signal(1:length(signal_EMG),1); % Nan data 삭제 (signal_EMG와 크기 일치)

    
    %% median filter
    
    % visual-picked threshold
    threshold = -1;
    
    % find data values above the threshold
    suprathresh = find( FSR_signal(:,2)>threshold );
    
    % initialize filtered signal
    filtsig = FSR_signal(:,2);
    
    % loop through suprathreshold points and set to median of k
    k = 20; % actual window is k*2+1
    for ti=1:length(suprathresh)
        
        % find lower and upper bounds
        lowbnd = max(1,suprathresh(ti)-k);
        uppbnd = min(suprathresh(ti)+k,length(FSR_signal));
        
        % compute median of surrounding points
        filtsig(suprathresh(ti)) = median(FSR_signal(lowbnd:uppbnd,2));
    end
    
    % flat selection
    TF1 = islocalmin(filtsig, 'FlatSelection', 'last');
    
    init_idx = zeros;
    t = 1; 
    for lenTF = 1 : length(TF1)
        if TF1(lenTF) == 1 
           init_idx(t,1) = lenTF;    % heel strike index before refining 
           init_idx(t,2) = FSR_signal(lenTF,1); 
           t = t+1;
        end
    end
    
    % threshold 보다 높은 값들 nan으로 지정
    for heelpoint = 1 : length(init_idx)
        if filtsig(init_idx(heelpoint,1)) >= 5 % threshold
            init_idx(heelpoint,:) = nan;
        end
    end
    heelstrikeidx = rmmissing(init_idx); % remove Nan elements
  
    % check with plot
    figure, clf
    plot(FSR_signal(:,1),FSR_signal(:,2), FSR_signal(:,1),filtsig, 'linew',2)
    hold on
    plot(FSR_signal(TF1,1),FSR_signal(TF1,2),'r*','linew',2)
    plot(FSR_signal(heelstrikeidx(:,1),1),FSR_signal(heelstrikeidx(:,1),2),'k*','linew',2)
    legend('FSR','FSR - filtered','Heel strike index', 'Final heel strike index')
    ylabel('FSR data','fontsize',11,'fontname','arial')
    xlabel('Time (sec)','fontsize',11,'fontname','arial')
    hold off
    
    %% IMU segmentation
    % IMU data interpolation 해서 0 구간 보정
    fignum = 1;
    for imunum = 1 : width(signal_IMU)
        if rem(imunum, 9) == 1
         figure;   
             fignum = 1;
        end
        
        subplot(3,3,fignum)        
        plot(signal_IMU_time(:,1), signal_IMU(:,imunum)); hold on
        
        [~,maxpeak] = findpeaks(signal_IMU(:,imunum), 'MinPeakDistance',srate*0.5); % 50deg/sec 이상, 0.5초 간격 이상
        temp = signal_IMU(1:maxpeak(end),imunum) == 0;  % maxpeak 길이 까지만 신호 끊기는 구간 detection
        temp(maxpeak(end):length(signal_IMU)) = zeros; % size 맞추기
        outliers = temp;  % temp를 outlier로 변경
        
        F = griddedInterpolant(signal_IMU_time(~outliers,1), signal_IMU(~outliers,imunum));
        signalRR = signal_IMU(:,imunum);
        signalRR(outliers) = F(signal_IMU_time(outliers,1));
        signal_IMU(:,imunum)=signalRR;
        
        plot(signal_IMU_time(:,1), signal_IMU(:,imunum), 'k-', 'linew', 1.5)
        plot(signal_IMU_time(maxpeak,1), signal_IMU(maxpeak,imunum),'gp','linew',1,'markersize',2,'markerfacecolor','b')
        
        fignum = fignum+1;
        clearvars maxpeak
    end
    legend('Raw signal','Interpolated')
    
    
    fs = length(signal_IMU)/signal_IMU_time(end,1);
    
    IMUterm(1,:) = 1:18:min(size(signal_IMU));
    IMUterm(2,:) = 10:18:min(size(signal_IMU));
    
    if foot ==1
        % 1st~5th column : EMG signal, 6th column : time segment
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 1, 3, 5, 7번 센서
            for j = 1 : 4
                segIMUL{i,j} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(1,j):IMUterm(1,j)+8);
            end
        end
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 11번 센서
            segIMUL{i,5} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(1,6):IMUterm(1,6)+8);
        end
        
        % time segmentation --> last column 추가
        temp_size=min(size(segIMUL))+1;
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Left) - 4 ch
            segIMUL{i,temp_size} = signal_IMU_time(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1);
        end
        
    elseif foot ==2
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 2, 4, 6, 8번 센서
            for j = 1 : 4
                segIMUR{i,j} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(2,j):IMUterm(2,j)+8);
            end
        end
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 12번 센서
            segIMUR{i,5} = signal_IMU(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1,IMUterm(2,6):IMUterm(2,6)+8);
        end
        
        % time segmentation --> last column 추가
        temp_size=min(size(segIMUR))+1;
        for i = 1 : length(heelstrikeidx)-1  % cycle 별 IMU신호 나눔 (Right) - 4 ch
            segIMUR{i,temp_size} = signal_IMU_time(heelstrikeidx(i,1):heelstrikeidx(i+1,1)-1);
        end
    end

    clear signal_IMU_time

    % EMG segmentation
    % EMG offset 제거
    for i = 1 : 8
        signal_EMG(:,i) = signal_EMG(:,i) - mean(signal_EMG(:,i));
    end

    % EMG gait cycle time index
    try
        t1=1; t2=1;
        for  i = 1 :length(signal_EMG_time)
            if round(signal_EMG_time(i),3) == round(heelstrikeidx(t2,2),3)
                heelstrikeidx_EMG(t1,1) = signal_EMG_time(i);
                heelstrikeidx_EMG(t1,2) = i;
                t1=t1+1;
                t2=t2+1;
            end
        end
    catch
    end
    
    % EMG gait cycle segmentation
    if foot==1
        for i = 1 : length(heelstrikeidx_EMG)-1  % cycle 별 EMG 신호 나눔 (Left)
            for j = 1:4
                segEMGL{i,j} = signal_EMG(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1,j);
            end
            segEMGL{i,5} = signal_EMG_time(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1);        % time segmentation --> last column 추가
        end
                % time segmentation --> last column 추가
    elseif foot ==2
        for i = 1 : length(heelstrikeidx_EMG)-1  % cycle 별 EMG 신호 나눔 (right)
            for j = 1:4
                segEMGR{i,j} = signal_EMG(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1,j);
            end
            segEMGR{i,5} = signal_EMG_time(heelstrikeidx_EMG(i,2):heelstrikeidx_EMG(i+1,2)-1);        % time segmentation --> last column 추가
        end
    end
    
    
end
toc
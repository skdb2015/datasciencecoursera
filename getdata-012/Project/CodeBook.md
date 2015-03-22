## Analysis Results code book
This file contains. A description of the variables, the data,. And. Any transformations or work that performed to clean up the data from the UCI Human. Activity Recognition dataset

### Study design
* Each row of the feature vector dataset (X_test.txt or X_train.txt) corresponds to each row of the subject dataset (subject_test.txt or subject_train.txt) and each row of the activity dataset (y_test.txt or y_train.txt). These sets of files are combined such that for each row that contains the 561 feature vector, the corresponding subject ID and activity for which the measurements wer calculated are present in the same row.

* The test and train datasets as built above are then stitched together to form a single dataset

* the activity codes which are indicated as numbers are replaced with the activity labels read from the activity_labels.txt file

* The variable names are built in an automated fashion by reading in the features.txt file and then processing the strings to be more descriptive

* The 561-element feature vector for each measurement was filtered based on whether it was a mean or a standard deviation measurement. It was done by evaulating if the raw variable name contained the string "-mean" (for mean measurements) or "-std" (for standard deviation measurements).

* For each unique (subject_ID, Activity) pair, the average of the mean and standard deviation measurements are computed, thus providing an average of the measurements for each activity by each subject.

### Code Book
1. subject_ID : 
    * The ID of the subjects. The range is from 1 through 30
2. Activity : 
    * The activity associated with that subject. There are 6 activities: 
        + WALKING
        + WALKING_UPSTAIRS
        + WALKING_DOWNSTAIRS
        + SITTING
        + STANDING
        + LAYING

3. Average_of_tBodyAcc_mean___X : (seconds) 
    * Average of body acceleration time mean measurements in the X-axis from the accelerometer

4. Average_of_tBodyAcc_mean___Y : (seconds) 
    * Average of body acceleration time mean measurements in the Y-axis from the accelerometer           

5. Average_of_tBodyAcc_mean___Z : (seconds) 
    * Average of body acceleration time mean measurements in the Z-axis from the accelerometer

6. Average_of_tBodyAcc_std___X : (seconds)
    * Average of body acceleration time standard deviation measurements in the X-axis from the accelerometer

7. Average_of_tBodyAcc_std___Y : (seconds)
    * Average of body acceleration time standard deviation measurements in the Y-axis from the accelerometer

8. Average_of_tBodyAcc_std___Z : (seconds) 
    * Average of body acceleration time standard deviation measurements in the Z-axis from the accelerometer

9. Average_of_tGravityAcc_mean___X : (seconds) 
    * Average of gravity acceleration time mean measurements in the X-axis from the accelerometer

10. Average_of_tGravityAcc_mean___Y : (seconds) 
    * Average of gravity acceleration time mean measurements in the Y-axis from the accelerometer

11. Average_of_tGravityAcc_mean___Z : (seconds) 
    * Average of gravity acceleration time mean measurements in the Z-axis from the accelerometer

12. Average_of_tGravityAcc_std___X : (seconds) 
    * Average of gravity acceleration time standard deviation measurements in the X-axis from the accelerometer

13. Average_of_tGravityAcc_std___Y : (seconds) 
    * Average of gravity acceleration time standard deviation measurements in the Y-axis from the accelerometer

14. Average_of_tGravityAcc_std___Z : (seconds) 
    * Average of gravity acceleration time standard deviation measurements in the Z-axis from the accelerometer

15. Average_of_tBodyAccJerk_mean___X : (seconds) 
    * Average of body jerk time mean measurements in the X axis from the accelerometer

16. Average_of_tBodyAccJerk_mean___Y : (seconds) 
    * Average of body jerk time mean measurements in the Y axis from the accelerometer 

17. Average_of_tBodyAccJerk_mean___Z : (seconds) 
    * Average of body jerk time mean measurements in the Z axis from the accelerometer

18. Average_of_tBodyAccJerk_std___X : (seconds) 
    * Average of body jerk time standard deviation measurements in the X axis from the accelerometer

19. Average_of_tBodyAccJerk_std___Y : (seconds) 
    * Average of body jerk time standard deviation measurements in the Y axis from the accelerometer 

20. Average_of_tBodyAccJerk_std___Z : (seconds) 
    * Average of body jerk time standard deviation measurements in the Z axis from the accelerometer

21. Average_of_tBodyGyro_mean___X : (seconds) 
    * Average of body acceleration time mean measurements in the X axis from the gyroscope

22. Average_of_tBodyGyro_mean___Y : (seconds)
    * Average of body acceleration time mean measurements in the Y axis from the gyroscope

23. Average_of_tBodyGyro_mean___Z : (seconds)
    * Average of body acceleration time mean measurements in the Z axis from the gyroscope

24. Average_of_tBodyGyro_std___X : (seconds) 
    * Average of body acceleration standard deviation measurements in the X axis from the gyroscope

25. Average_of_tBodyGyro_std___Y : (seconds) 
    * Average of body acceleration standard deviation measurements in the Y axis from the gyroscope

26. Average_of_tBodyGyro_std___Z : (seconds) 
    * Average of body acceleration standard deviation measurements in the Z axis from the gyroscope

27. Average_of_tBodyGyroJerk_mean___X : (seconds) 
    * Average of body jerk time mean measurements in the X axis from the gyrosocope 

28. Average_of_tBodyGyroJerk_mean___Y : (seconds) 
    * Average of body jerk time mean measurements in the Y axis from the gyrosocope 

29. Average_of_tBodyGyroJerk_mean___Z : (seconds) 
    * Average of body jerk time mean measurements in the Z axis from the gyrosocope 

30. Average_of_tBodyGyroJerk_std___X : (seconds) 
    * Average of body jerk time mean measurements in the X axis from the gyroscope

31. Average_of_tBodyGyroJerk_std___Y : (seconds) 
    * Average of body jerk time mean measurements in the Y axis from the gyroscope

32. Average_of_tBodyGyroJerk_std___Z : (seconds) 
    * Average of body jerk time mean measurements in the Z axis from the gyroscope

33. Average_of_tBodyAccMag_mean__ : (seconds) 
    * Average of body acceleration time magnitude mean measurements from the accelerometer

34. Average_of_tBodyAccMag_std__ : (seconds) 
    * Average of body acceleration time magnitude standard deviation measurements from the accelerometer

35. Average_of_tGravityAccMag_mean__ : (seconds) 
    * Average of gravity acceleration time magnitude mean measurements from the accelerometer

36. Average_of_tGravityAccMag_std__ : (seconds) 
    * Average of gravity acceleration time magnitude standard deviation measurements from the accelerometer

37. Average_of_tBodyAccJerkMag_mean__ : (seconds) 
    * Average of body jerk time magnitude mean measurements from the accelerometer

38. Average_of_tBodyAccJerkMag_std__ : (seconds) 
    * Average of body jerk time magnitude standard deviation measurements from the accelerometer

39. Average_of_tBodyGyroMag_mean__ : (seconds) 
    * Average of body acceleration time magnitude mean measurements from the gyroscope

40. Average_of_tBodyGyroMag_std__ : (seconds) 
    * Average of body acceleration time magnitude mean measurements from the gyroscope

41. Average_of_tBodyGyroJerkMag_mean__ : (seconds)
    * Average of body jerk time magnitude mean measurements from the gyroscope

42. Average_of_tBodyGyroJerkMag_std__ : (seconds)
    * Average of body jerk time magnitude standard deviation measurements from the gyroscope

43. Average_of_fBodyAcc_mean___X : (Hz) 
    * Average of body acceleration frequency mean measurements on the X axis from the accelerometer 

44. Average_of_fBodyAcc_mean___Y : (Hz) 
    * Average of body acceleration frequency mean measurements on the Y axis from the accelerometer 

45. Average_of_fBodyAcc_mean___Z : (Hz) 
    * Average of body acceleration frequency mean measurements on the Z axis from the accelerometer 

46. Average_of_fBodyAcc_std___X : (Hz) 
    * Average of body acceleration frequency standard deviation measurements on the X axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

47. Average_of_fBodyAcc_std___Y : (Hz) 
    * Average of body acceleration frequency standard deviation measurements on the Y axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

48. Average_of_fBodyAcc_std___Z : (Hz) 
    * Average of body acceleration frequency standard deviation measurements on the Z axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

49. Average_of_fBodyAcc_meanFreq___X : (Hz) 
    * Average of body acceleration frequency mean frequency measurements on the X axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

50. Average_of_fBodyAcc_meanFreq___Y : (Hz) 
    * Average of body acceleration frequency mean frequency measurements on the Y axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

51. Average_of_fBodyAcc_meanFreq___Z : (Hz) 
    * Average of body acceleration frequency mean frequency measurements on the Z axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

52. Average_of_fBodyAccJerk_mean___X : (Hz) 
    * Average of body jerk frequency mean measurements on the X axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

53. Average_of_fBodyAccJerk_mean___Y : (Hz) 
    * Average of body jerk frequency mean measurements on the Y axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

54. Average_of_fBodyAccJerk_mean___Z : (Hz) 
    * Average of body jerk frequency mean measurements on the Z axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

55. Average_of_fBodyAccJerk_std___X : (Hz)
    * Average of body jerk frequency standard deviation measurements on the X axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

56. Average_of_fBodyAccJerk_std___Y : (Hz) 
    * Average of body jerk frequency standard deviation measurements on the Y axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

57. Average_of_fBodyAccJerk_std___Z : (Hz) 
    * Average of body jerk frequency standard deviation measurements on the Z axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

58. Average_of_fBodyAccJerk_meanFreq___X : (Hz) 
    * Average of body jerk frequency mean frequency measurements on the X axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

59. Average_of_fBodyAccJerk_meanFreq___Y : (Hz) 
    * Average of body jerk frequency mean frequency measurements on the Y axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

60. Average_of_fBodyAccJerk_meanFreq___Z : (Hz) 
    * Average of body jerk frequency mean frequency measurements on the Z axis from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

61. Average_of_fBodyGyro_mean___X : (Hz) 
    * Average of body accleration frequency mean measurements on the X axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

62. Average_of_fBodyGyro_mean___Y : (Hz) 
    * Average of body accleration frequency mean measurements on the Y axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

63. Average_of_fBodyGyro_mean___Z : (Hz) 
    * Average of body accleration frequency mean measurements on the Z axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

64. Average_of_fBodyGyro_std___X : (Hz) 
    * Average of body accleration frequency standard deviation measurements on the X axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

65. Average_of_fBodyGyro_std___Y : (Hz) 
    * Average of body accleration frequency standard deviation measurements on the Y axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

66. Average_of_fBodyGyro_std___Z : (Hz) 
    * Average of body accleration frequency standard deviation measurements on the Z axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

67. Average_of_fBodyGyro_meanFreq___X : (Hz) 
    * Average of body accleration frequency mean frequency measurements on the X axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

68. Average_of_fBodyGyro_meanFreq___Y : (Hz) 
    * Average of body accleration frequency mean frequency measurements on the Y axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

69. Average_of_fBodyGyro_meanFreq___Z : (Hz) 
    * Average of body accleration frequency mean frequency measurements on the Z axis from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

70. Average_of_fBodyAccMag_mean__ : (Hz) 
    * Average of body acceleration frequency magnitude mean measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal 

71. Average_of_fBodyAccMag_std__ : (Hz) 
    * Average of body acceleration frequency magnitude standard deviation measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

72. Average_of_fBodyAccMag_meanFreq__ : (Hz) 
    * Average of body acceleration frequency magnitude mean frequency measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

73. Average_of_fBodyBodyAccJerkMag_mean__ : (Hz) 
    * Average of body jerk frequency magnitude mean measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

74. Average_of_fBodyBodyAccJerkMag_std__ : (Hz) 
    * Average of body jerk frequency magnitude standard deviation measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

75. Average_of_fBodyBodyAccJerkMag_meanFreq__ : (Hz) 
    * Average of body jerk frequency magnitude mean frequency measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

76. Average_of_fBodyBodyGyroMag_mean__ : (Hz) 
    * Average of body acceleration frequency magnitude mean measurements from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal 

77. Average_of_fBodyBodyGyroMag_std__ : (Hz) 
    * Average of body acceleration frequency magnitude standard deviation measurements from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal 

78. Average_of_fBodyBodyGyroMag_meanFreq__ : (Hz) 
    * Average of body acceleration frequency magnitude mean frequency measurements from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal 

79. Average_of_fBodyBodyGyroJerkMag_mean__ : (Hz) 
    * Average of body jerk frequency magnitude mean measurements from the gyroscope after applying a Fast Fourier Transform (FFT) on the signal

80. Average_of_fBodyBodyGyroJerkMag_std__ : (Hz) 
    * Average of body jerk frequency magnitude standard deviation measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal

81. Average_of_fBodyBodyGyroJerkMag_meanFreq__ : (Hz) 
    * Average of body jerk frequency magnitude mean frequency measurements from the accelerometer after applying a Fast Fourier Transform (FFT) on the signal


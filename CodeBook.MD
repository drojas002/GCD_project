
The script run_analysis.R perform the following actions

1. Merges the training and the test sets to create one data set.

- Read ./train/X_train.txt file and asign it to train_data variable
- Read ./train/y_train.txt file and asign it to train_label variable
- Read ./train/subject_train.txt file and asign it to train_subject variable
- Read ./train/X_test.txt file and asign it to test_data variable
- Read ./train/y_test.txt file and asign it to test_label variable
- Read ./train/subject_test.txt file and asign it to test_subject variable
- join the train_data and test_data to store it in data
- join the train_label and test_label to store it in label
- join the train_subject and test_subject to store it in subject

2. Extracts only the measurements on the mean and standard deviation for each measurement.
 
- Read ./features.txt file and asign it to features variable
- extract the patterns that match to "mean" or "std" related respectively to mean and standard deviation mesrurements, to generate an index vector and store it in meanSTD_i
- subset data with meanSTD_i vector
- set the colum names of data assigning the variable features to data column names

3. Uses descriptive activity names to name the activities in the data set

- Read ./activity_label.txt file and asign it to train_data variable
- clean de data an extract just the second column from activity
- set activity column name
- set subject column name 

4. Appropriately labels the data set with descriptive variable names.

- get a new data frame called dataSet by combining subject, label and data variables

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


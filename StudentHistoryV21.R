#load required packages  
library(arulesSequences)
library(tidyr)
library(stringr)

### Create main data sheet to perform analysis(Student Data)

# load CSV course history and course number change list
student_course_reg_mod <- read.csv("student_course_reg_mod.csv") 

# add course cn variable --- has subject number and course name combined
student_course_reg_mod$course_cn <- with(student_course_reg_mod,paste(subject_num, course, sep = ": "))

# remove all commas from courses - needed to choose predictions from rhs without errrors
student_course_reg_mod$course_cn <- gsub(","," ",as.character(student_course_reg_mod$course_cn))

# remove all of the course information - focus on student attributes
student_data <- student_course_reg_mod[,c(1,3,5,6,9,10,13,15,17,19,35)]

# make student ids characters rather than integers and make degree codes a factor
student_data$student_unique_identifier <- as.character(student_data$student_unique_identifier)
student_data$student_degree_code <- as.factor(student_data$student_degree_code)

#create student_data with registration sequencing number (1-9)
regvalues <- (1:9)
regterms <- unique(student_data$register_term_desc)
student_data$reg_code <- regvalues[match(student_data$register_term_desc, regterms)]

#---------------------------------------------


### choose semester for recommendation & a random student
#select random semester (does not include 1st semester becuase no prior history)
semester <- sample(3:9,1)
chosen_semester <- subset(student_data, reg_code == semester)

#select a random student
id <- sample(unique(chosen_semester$student_unique_identifier),1)
#extract all course history of that student
student <- subset(student_data, student_unique_identifier == id) 
student$course_cn <- factor(student$course_cn)
student <- student[order(student$reg_code),]

# create list for all courses available for the current semester 
courses_available <- as.character(unique(chosen_semester$course_cn))

#check if the student has a prior history --- if the student has taken any courses already 
spec_student_semrange   <- semester - min(student$reg_code)
spec_student_semrange

#if student has 
if(spec_student_semrange >= 1) {
    
#grab all of the student's history prior to the selected semester  
prior_stud_hist <- subset(student, student$reg_code %in% 1:(semester-1)) 

#extract attributes from the chosen student's current most current semester 
attributes <- student[nrow(student), c("student_college_group_desc", "student_degree_code", "student_major_desc", "student_minor_code", "student_level_desc","student_type_desc")]

#select all student that match the attributes of that student (does not include student)
student_selection <- subset(student_data,student_unique_identifier!= id & student_college_group_desc == attributes$student_college_group_desc & student_degree_code==attributes$student_degree_code & student_major_desc == attributes$student_major_desc & reg_code %in% 1:(semester-1))

# create list of all courses that similar students have taken that are avaible this semester, but remove all of the courses that our student has taken
courses_available_student <- factor(courses_available[which(courses_available %in% student_selection$course_cn)])
courses_available_student <- factor(courses_available_student[which(!courses_available_student %in% prior_stud_hist$course_cn)])

### find students that took one of the courses that our chosen student in last year or last semester
# if student history prior to current semester is 1 then use one semester if more than one than subset by 2 semesters 
last_semest_courses <- if(max(prior_stud_hist$reg_code) - min(prior_stud_hist$reg_code) == 1){prior_stud_hist[prior_stud_hist$reg_code == max(prior_stud_hist$reg_code),]
} else {prior_stud_hist[prior_stud_hist$reg_code %in% (max(prior_stud_hist$reg_code)-1):max(prior_stud_hist$reg_code),]}

# subset students who took the same courses
took_samecourse <-subset(student_selection, course_cn %in% last_semest_courses$course_cn)
close_students<- unique(took_samecourse$student_unique_identifier)

students_like_me <- subset(student_selection, student_unique_identifier %in% close_students)

# TEST --- use only students that took one of same courses that selected student took last semester 
student_selection <- students_like_me
student_selection$course_cn <- factor(student_selection$course_cn)

#only student id, sequence and course info
student_trans_data <- student_selection[,c(1,12,11)]

# remove duplicates and order by studentid
student_trans_data <- student_trans_data[!duplicated.data.frame(student_trans_data),]
student_trans_data <- student_trans_data[order(student_trans_data$student_unique_identifier),]

#create temporary csv file that will be turned into transaction data
write.table(student_trans_data, "student_trans.csv", sep="\t", row.names=FALSE, col.names=FALSE) 

#create transacstion data - change column names to sequenceid and eventid
class_basket <- read_baskets("student_trans.csv",sep = "\t",info =  c("sequenceID","eventID"))

#load data into cspade --- used inspect() to see rules 
student_rules <- cspade(class_basket, parameter = list(support = 0.02, maxsize = 4, maxlen = 4), control = list(verbose = TRUE))

#remove all ' "\ ' from item labels so that they can be subset
itemLabels(student_rules) <- gsub('\"','',itemLabels(student_rules))

# places confidence rules on data and changes class to "sequencerules" which is needed to subset by lhs or rhs sides
student_rules_info<- ruleInduction(student_rules, confidence = 0.3,control = list(verbose = TRUE))

# chooses courses that our students have taken that are part of the rules (LHS or RHS)
student_course_list <- as.character(unique(prior_stud_hist$course_cn))
student_course_lsemester <- as.character(unique(last_semest_courses$course_cn))
student_list_sub <- which(student_course_list %in% itemLabels(student_rules))
student_lcourse_list_sub <- which(student_course_lsemester %in% itemLabels(student_rules))

#subsets the lhs so that it at least includes courses that our selected student has already taken  

sub1 <- subset(student_rules_info, lhs(student_rules_info) %in% student_course_lsemester[student_lcourse_list_sub] & lift > 1.2)
sub1 <- subset(sub1, !rhs(sub1) %in% student_course_list[student_list_sub])

srule_df <- as(sub1, "data.frame")
    
#makes it so that the lhs and rhs are seprate columns 
if(nrow(srule_df) > 0){
    srule_df <- separate(data = srule_df, col = rule, into = c("lhs", "rhs"), sep ="=>")
    srule_df <- srule_df[with(srule_df, order(-support, -lift)), ]
} else{srule_df <- 0}

rec <- unique(srule_df$rhs)
rec <- unlist(str_split(rec,","))
rec <- gsub(" <{","",rec, perl = TRUE)
rec <- gsub("}>","",rec, perl = TRUE)
rec <- rec[!duplicated(rec)]
rec <- intersect(rec,courses_available)

rec <- if(length(rec) <= 5){rec <- rec
} else if(length(rec)> 5){
    rec <- rec[1:5]}

per_corr <- length(intersect(student[student$reg_code %in% semester,11],rec))/length(student[student$reg_code %in% semester,11])
per_corr_life <- length(intersect(student[student$reg_code %in% semester:9,11],rec))/length(rec)

cat("Similar students would take these courses:")
print(rec)
cat("These recommendations were correct for the semester:")
print(intersect(student[student$reg_code %in% semester,11],rec))
cat("The recommendations were", per_corr, "accurate for this semester.")
cat("The recommendations were", per_corr_life, "accurate if semesters up to Spring 2014 are included.")

} else{ 
#select all student that match the attributes of that student (does not include student)
attributes <- subset(student, reg_code == semester)[1, c("student_college_group_desc", "student_degree_code", 
                                                         "student_major_desc", "student_minor_code", "student_level_desc","student_type_desc")]
#select all student that match the attributes of that student (does not include student)
student_selection <- subset(student_data,student_unique_identifier!= id & student_college_group_desc == attributes$student_college_group_desc & student_degree_code==attributes$student_degree_code & student_type_desc == attributes$student_type_desc & reg_code %in% 1:(semester-1))
student_selection$course_cn <- factor(student_selection$course_cn)
arule_stud_data <- student_selection[,c(1,11)]
arule_stud_data <- arule_stud_data[!duplicated.data.frame(arule_stud_data),]

aggrData <- split(arule_stud_data[,"course_cn"], arule_stud_data[,"student_unique_identifier"])

st_TrnsData <- as(aggrData, "transactions")

rules = apriori(st_TrnsData, parameter=list(support=0.05, confidence=0.3))

ar_rules <- subset(rules, lift > 1.2)
ar_rules
inspect(ar_rules)
}

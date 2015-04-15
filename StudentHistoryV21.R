#load required packages  
require(arulesSequences)
require(tidyr)

### Create main data sheet to perform analysis(Student Data)

# load CSV course history and course number change list
student_course_reg_mod <- read.csv("student_course_reg_mod.csv") 

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
semester <- sample(2:9,1)
chosen_semester <- subset(student_data, reg_code == semester)

#select a random student
id <- sample(unique(chosen_semester$student_unique_identifier),1)

#extract all course history of that student
student <- subset(student_data, student_unique_identifier == id) 
student$course_cn <- factor(student$course_cn)
student <- student[order(student$reg_code),]

#make sure student has a prior history 
spec_student_semrange   <- semester - min(student$reg_code)

if(spec_student_semrange >= 1) {
    
#grab all of the student's history prior to the selected semester  
prior_stud_hist <- subset(student, student$reg_code %in% 1:(semester-1)) 

#extract attributes from the chosen student's current most current semester 
attributes <- student[nrow(student), c("student_college_group_desc", "student_degree_code", "student_major_desc", "student_minor_code", "student_level_desc")]

#select all student that match the attributes of that student (does not include student)
student_selection <- subset(student_data,student_unique_identifier!= id & student_college_group_desc == attributes$student_college_group_desc & student_degree_code==attributes$student_degree_code & student_major_desc == attributes$student_major_desc & reg_code %in% 1:semester)

# create list for all courses available for the current semester 
courses_available <- factor(unique(chosen_semester$course_cn))

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
student_rules <- cspade(class_basket, parameter = list(support = 0.04), control = list(verbose = TRUE))

#remove all ' "\ ' from item labels so that they can be subset
itemLabels(student_rules) <- gsub('\"','',itemLabels(student_rules))

# places confidence rules on data and changes class to "sequencerules" which is needed to subset by lhs or rhs sides
student_rules_info<- ruleInduction(student_rules, confidence = 0.25,control    = list(verbose = TRUE))

# chooses courses that our students have taken that are part of the rules (LHS or RHS)
student_course_list <- as.character(unique(prior_stud_hist$course_cn))
student_course_lsemester <- as.character(unique(last_semest_courses$course_cn))
student_list_sub <- which(student_course_list %in% itemLabels(student_rules))
student_lcourse_list_sub <- which(student_course_lsemester %in% itemLabels(student_rules))

#subsets the lhs so that it at least includes courses that our selected student has already taken  
specific_student_rules <- as(subset(student_rules_info, lhs(student_rules_info) %in% student_course_list[student_list_sub]), "data.frame")

lockey <- subset(student_rules_info, lhs(student_rules_info) %in% student_course_lsemester[student_lcourse_list_sub])
okey <- subset(lockey, !rhs(lockey) %in% student_course_list[student_list_sub])

hops <- as(lockey, "data.frame")
ice <- as(okey, "data.frame")
    
#makes it so that the lhs and rhs are seprate columns 
specific_student_rules <- separate(data = specific_student_rules, col = rule, into = c("lhs", "rhs"), sep ="=>")
specific_student_rules <- specific_student_rules[order(specific_student_rules$lift, decreasing = T), ]

if(nrow(ice) > 0){
    ice <- separate(data = ice, col = rule, into = c("lhs", "rhs"), sep ="=>")
    ice <- ice[with(ice, order(-support, -lift)), ]
} else{ice <- 0}

if(nrow(hops) >0 ){
    hops <- separate(data = hops, col = rule, into = c("lhs", "rhs"), sep ="=>")
    hops <- hops[order(hops$lift, decreasing = T), ]
} else{hops <-0}

if(length(unique(ice$rhs)) > 0 & length(unique(ice$rhs))<= 5){
    rec <- unique(ice$rhs)[1:length(unique(ice$rhs))]
} else if(length(unique(ice$rhs) > 5)) {
    rec <- unique(ice$rhs)[1:5]
} else { print("Sorry no reccomendation :(")}
rec <- gsub(" <{","",rec, perl = TRUE)
rec <- gsub("}>","",rec, perl = TRUE)
per_corr <- length(intersect(student[student$reg_code %in% semester,11],rec))/length(rec)
per_corr_life <- length(intersect(student[student$reg_code %in% semester:9,11],rec))/length(rec)

cat("Similar students would take these courses:")
print(rec)
cat("These recommendations were correct for the semester:")
print(intersect(student[student$reg_code %in% semester,11],rec))
cat("The recommendations were", per_corr, "accurate for this semester.")
cat("The recommendations were", per_corr_life, "accurate if semesters up to Spring 2014 are included.")

} else{print ("Need to create basket for new students")}

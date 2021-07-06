/*
** April 27, 2019
** Helen Li
** 
** This program distinguishes the style of a single-family house based on external descriptions.  
** Prompts yes/no questions at the user and use their answers to identify the style of the house
** they are looking at. 
** 
** It currently includes 14 different single-family house styles. 
** The program ends immediately when a guess is made or when it cannot find a match. 
*/

; clears everything and resets to initial fact so that the module is empty and ready to run 
(clear)
(reset) 

; batches the tools.clp
(batch h/tools.clp)

/* 
** Parses through all elements of the param ?list and 
** dynamically creates backward-chained facts for every value  
** in ?list by using the eval function. 
**
** For example, if ?list has the value called "bay-windows", 
** the function will create this as a command:
** (do-backward-chaining bay-windows) 
*/ 
(deffunction backwards (?list)
   (for (bind ?i 2) (<= ?i (length$ ?list)) (++ ?i)
      (eval (str-cat "(do-backward-chaining " (nth$ ?i ?list) ")"))
   )

   (return)
) 

(defglobal ?*WINDOW* = (create$ Does-it-have bay-windows eyebrow-windows leaded-windows  ; named WINDOW because it denotes types of windows
                                             double-hung-windows half-circle-windows
                                             clerestory-windows))

(defglobal ?*ROOF* = (create$ Does-it-have-a mansard-roof conventional-gable-roof        ; named ROOF because it denotes types of roofs
                                             low-gable-roof hip-roof flat-roof 
                                             steep-gable-roof shed-roof))
                              
(defglobal ?*HEIGHT* = (create$ Is-it 1-story 2-stories 3-stories))                      ; named HEIGHT because it denotes ranges of height

(defglobal ?*MATERIAL* = (create$ Is-it-made-of cedar-shingles clapboard steel           ; named MATERIAL because it denotes types of materials
                                                stone brick wood glass))                         

(defglobal ?*EXTERIOR* = (create$ Does-it-have tower turret dormer entrance-porch        ; named EXTERIOR because it denotes special external parts
                                               chimney infill-panels knee-braces
                                               carved-door wooden-door pilasters
                                               columns arches)) 
 
(defglobal ?*SHAPE* = (create$ Is-it rectangular L-shaped U-shaped round boxy))          ; named SHAPE because it denotes types of house shapes                   

(defglobal ?*COLOR* = (create$ Is-the-color white marble-like earthly-tones))            ; named COLOR because it denotes types of colors  

(defglobal ?*OTHER* = (create$ Is-it symmetrical good-for-hot-climate overhang           ; named OTHER because it denotes all things that do not
                                     related-to-nature mostly-windows))                  ; belong to any of the other categories above 
                         
(defglobal ?*LINE* = "
")                                                                                       ; stores a line break to simplify the dynamic rule building process
(defglobal ?*QUOTE* = "\"")                                                              ; stores one double quote (") to simplify the dynamic rule building process  

; these commands deal with backward chaining all the facts 
(backwards ?*WINDOW*)
(backwards ?*ROOF*) 
(backwards ?*HEIGHT*) 
(backwards ?*MATERIAL*) 
(backwards ?*EXTERIOR*) 
(backwards ?*SHAPE*) 
(backwards ?*OTHER*) 
(backwards ?*COLOR*) 

/* 
** Based on the param ?in, this function builds the right hand side of 
** the forward chaining rules that finds the styles of houses. 
** 
** For example, if the argument ?in is VICTORIAN, the function will build this:
** (assert (guessed)) 
** (printout t "VICTORIAN" crlf)
*/
(deffunction right (?in)
   (bind ?right (str-cat "(assert (guessed))" ?*LINE* "(printout t " ?*QUOTE* ?in ?*QUOTE* " crlf)"))
   (build ?right) 

   (return)
) 

/********************************
** 
** Forward Chaining Rules for House Styles, with only exterior traits. 
** Searches for specific patterns and confirms whether
** a certain style can be matched to the traits given. 
** 
** Asserts (guessed) whenever a guess has been made, which will be
** helpful for the guess-made rule to know then to end the game. 
*/
(defrule victorian "Is the style Victorian"
   (or (2-stories yes) (3-stories yes))
   (symmetrical no)
   (or (mansard-roof yes) (conventional-gable-roof yes))
   (or (bay-windows yes) (eyebrow-windows yes))
   (or (tower yes) (turret yes) (dormer yes))
   (entrance-porch yes) 
=>
   (right VICTORIAN)
) 

(defrule gothic-revival "Is the style Gothic Revival"
   (or (2-stories yes) (3-stories yes))
   (or (steep-gable-roof yes) (mansard-roof yes))
   (or (bay-windows yes) (leaded-windows yes))
   (chimney yes)
   (entrance-porch yes) 
   (knee-braces yes)
=>
   (right GOTHIC-REVIVAL)
) 

(defrule colonial "Is the style Colonial"
   (2-stories yes)
   (symmetrical yes) 
   (or (hip-roof yes) (conventional-gable-roof yes) (low-gable-roof yes)) 
   (or (double-hung-windows yes) (leaded-windows yes))
   (cedar-shingles yes)
   (entrance-porch yes)  
   (or (columns yes) (pilasters yes))
=>
   (right COLONIAL)
) 

(defrule renaissance-revival "Is the style Renaissance Revival"
   (or (2-stories yes) (3-stories yes))
   (symmetrical yes) 
   (or (hip-roof yes) (flat-roof yes) (low-gable-roof yes))
   (or (half-circle-windows yes) (arches yes))
   (entrance-porch yes)
   (or (pilasters yes) (columns yes))
   (or (stone yes) (brick yes) (wood yes))
=>
   (right RENAISSANCE-REVIVAL)
) 

(defrule tudor "Is the style Tudor"
   (or (2-stories yes) (3-stories yes))
   (or (conventional-gable-roof yes) (steep-gable-roof yes))
   (leaded-windows yes) 
   (wooden-door yes) 
   (infill-panels yes)
=>
   (right TUDOR)
) 

(defrule greek-revival "Is the style Greek Revival"
   (or (2-stories yes) (1-story yes))
   (symmetrical yes) 
   (or (flat-roof yes) (low-gable-roof yes) (hip-roof yes)) 
   (entrance-porch yes)  
   (marble-like yes)
   (or (columns yes) (pilasters yes))
=>
   (right GREEK-REVIVAL)
) 

(defrule cape-cod "Is the style Cape Cod"
   (or (1-story yes) (2-stories yes))
   (symmetrical yes)
   (or (conventional-gable-roof yes) (low-gable-roof yes))
   (or (double-hung-windows yes) (leaded-windows yes))
   (or (cedar-shingles yes) (clapboard yes))
   (chimney yes)
=>
   (right CAPE-COD)
) 

(defrule english-cottage "Is the style English Cottage"
   (or (1-story yes) (2-stories yes))
   (symmetrical no)
   (or (steep-gable-roof yes) (conventional-gable-roof yes))
   (leaded-windows yes)
   (cedar-shingles yes)
   (chimney yes)
   (entrance-porch yes) 
   (related-to-nature yes)
=>
   (right ENGLISH-COTTAGE)
) 

(defrule ranch "Is the style Ranch"
   (or (1-story yes) (2-stories yes))
   (symmetrical no) 
   (or (low-gable-roof yes) (shed-roof yes) (hip-roof yes) (flat-roof yes))
   (double-hung-windows yes)
   (or (rectangular yes) (L-shaped yes) (U-shaped yes))  
   (overhang yes) 
=>
   (right RANCH)
) 

(defrule pueblo-revival "Is the style Pueblo Revival"
   (or (1-story yes) (2-stories yes))
   (symmetrical no)
   (or (low-gable-roof yes) (flat-roof yes))
   (round yes)
   (earthly-tones yes)
   (wooden-door yes)  
   (good-for-hot-climate yes)
=>
   (right PUEBLO-REVIVAL)
) 

(defrule craftsman "Is the style Craftsman"
   (or (1-story yes) (2-stories yes))
   (or (conventional-gable-roof yes) (low-gable-roof yes)) (flat-roof no)
   (double-hung-windows yes)
   (dormer yes) 
   (entrance-porch yes) 
   (columns yes)
   (knee-braces yes)
=>
   (right CRAFTSMAN) 
) 

(defrule spanish "Is the style Spanish"
   (or (1-story yes) (2-stories yes))
   (or (low-gable-roof yes) (flat-roof yes) (hip-roof yes))
   (half-circle-windows yes) 
   (or (white yes) (earthly-tones yes))
   (and (carved-door yes) (wooden-door yes))  
=>
   (right SPANISH)
) 

(defrule modern "Is the style Modern"
   (or (low-gable-roof yes) (flat-roof yes))
   (or (glass yes) (steel yes) (concrete yes)) 
   (mostly-windows yes)
   (boxy yes)
=>
   (right MODERN) 
) 

(defrule prairie "Is the style Prairie"
   (or (1-story yes) (2-stories yes))
   (or (hip-roof yes) (flat-roof yes))
   (clerestory-windows yes)
   (or (brick yes) (stone yes) (wood yes)) 
   (chimney yes)
   (overhang yes) 
=>
   (right PRAIRIE) 
) 

/********************************
** 
** Forward Chaining Rules to assert traits that can be certain 
** according to the information currently known. 
*/
(defrule height1 "If it is 1 story tall, then it must ..."
   (1-story yes)
=>
   (assert (2-stories no)) 
   (assert (3-stories no)) 
)

(defrule height2 "If it is 2 stories tall, then it must ..."
   (2-stories yes)
=>
   (assert (1-story no))
   (assert (3-stories no))  
)

(defrule height3 "If it is 3 stories tall, then it must ..."
   (3-stories yes)
=>
   (assert (1-story no))
   (assert (2-stories no))  
)

(defrule exHeight1 "If it is neither 2 nor 3 stories tall, then it must ..."
   (2-stories no) (3-stories no)
=>
   (assert (1-story no)) 
)

(defrule exHeight2 "If it is neither 1 nor 3 stories tall, then it must ..."
   (1-story no) (3-stories no) 
=>
   (assert (2-stories yes))
)

(defrule exHeight3 "If it is neither 1 nor 2 stories tall, then it must ..."
   (1-story no) (2-stories no) 
=>
   (assert (3-stories yes))
)

(defrule round-shape "If it has a round shape, then it must ..." 
   (round yes) 
=> 
   (assert (rectangular no)) 
   (assert (boxy no)) 
) 

(defrule rectangle-shape "If it has a rectangle shape, then it must ..." 
   (rectangular yes) 
=> 
   (assert (round no)) 
   (assert (boxy yes)) 
) 

(defrule box-shape "If it has a box shape, then it must ..." 
   (boxy yes) 
=> 
   (assert (round no)) 
) 

(defrule marble "If it is marble-like, then it must ..." 
   (marble-like yes) 
=> 
   (assert (white yes))
)

(defrule conventional-gable "If it has a conventional gable roof, then it must ..."
   (conventional-gable-roof yes)
=>
   (assert (low-gable-roof no)) 
   (assert (shed-roof no)) 
)

(defrule low-gable "If it has a low gable roof, then it must ..."
   (low-gable-roof yes)
=>
   (assert (conventional-gable-roof no)) 
   (assert (steep-gable-roof no))
)

(defrule flat-or-shed "If it has a flat or a shed roof, then it must ..."
   (or (flat-roof yes) (shed-roof yes))
=>
   (assert (conventional-gable-roof no)) 
   (assert (mansard-roof no)) 
   (assert (low-gable-roof no)) 
   (assert (hip-roof no)) 
   (assert (steep-gable-roof no))
)

(defrule flat "If it has a hip roof, then it must ..."
   (hip-roof yes)
=>
   (assert (conventional-gable-roof no)) 
   (assert (mansard-roof no)) 
   (assert (low-gable-roof no)) 
   (assert (flat-roof no)) 
   (assert (shed-roof no)) 
   (assert (steep-gable-roof no))
)

(defrule mansard "If it has a mansard roof, then it must ..."
   (mansard-roof yes)
=>
   (assert (low-gable-roof no)) 
   (assert (flat-roof no)) 
   (assert (shed-roof no)) 
   (assert (hip-roof no))
)

/********************************
** 
** A series of methods used for dynamic creation of
** backward chaining rules. 
*/

/*
** Parses through all elements of the param ?list and 
** dynamically creates a rule for every value in ?list 
** by calling the dynam-rule function. 
*/ 
(deffunction parse-for-dynam (?list)
   (for (bind ?i 2) (<= ?i (length$ ?list)) (++ ?i)
      (dynam-rule (nth$ ?i ?list) (nth$ 1 ?list))
   )

   (return)
) 

/*
** Dynamically builds a rule given two inputs, the first param ?in
** represents a certain trait of a house, and the second param ?name 
** represents the first element of the list the trait is coming from 
** (used to determine what kind of questions to ask). 
**
** The algorithm creates a string that looks exactly like a rule in JESS syntax 
** and uses the build function to process it as if it is a block of JESS code. 
** 
** For example, if the argument ?in is rectangular, and ?name is Is-it,  
** the functions will build this rule:
** (defrule need-rectangular-rule 
**    (need-rectangular ?) 
** => 
**    (bind ?x (askQ "Is it rectangular"))
**    (assert (rectangular ?x)) 
** )
*/
(deffunction dynam-rule (?in ?name) 
   (bind ?rule (str-cat "(defrule need-" ?in "-rule" ?*LINE* 
                        "(need-" ?in " ?)" ?*LINE*
                        "=>" ?*LINE*
                        "(bind ?x (askQ " ?*QUOTE* (check-hyphen ?name) " " (check-hyphen ?in) ?*QUOTE* "))" ?*LINE*  
                        "(assert (" ?in " ?x))" ?*LINE* 
                        ")")
   )

   (build ?rule)

   (return)
)

/*
** Checks for hyphens in the type of traits because all traits greater 
** than one word will have its words be separated by hyphens. 
** 
** Eliminates the hyphens to be white spaces so they can be used
** for the dynam-rule function to create better-looking rules. 
*/ 
(deffunction check-hyphen (?in)
   (bind ?x "") 
   (bind ?list (str-split ?in))                                              ; splits ?x into a list of characters 
   (bind ?hyphen 1)                                                          ; index of hyphen starts at 1, will not change if no hyphen are found 

   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i)                          ; iterates through all characters of ?x

      (bind ?curr (nth$ ?i ?list))                                           ; ?curr means current character 

      (if (eq ?curr "-") then                                                ; case where ?curr is a hyphen 
         (bind ?currIndex ?i)                                                ; local variable so that modifications do not affect the loop counter 
         (bind ?x (str-cat ?x (sub-string ?hyphen (-- ?currIndex) ?in) " ")) ; from previous hyphen index to ?currIndex-1 because ?currIndex is most recent hyphen index 
         
         (bind ?hyphen (+ 2 ?currIndex))                                     ; ?hyphen now represents the index right after the most recent hyphen found 
      )
   )
   
   (if (eq ?x "") then (bind ?x ?in)                                         ; case with no hyphens in ?x, so ?trait will be equal to ?x
    else (bind ?x (str-cat ?x (sub-string ?hyphen (length$ ?list) ?in)))     ; case with hyphens, this line concatenates the previous hyphen index of end of ?x 
   )

   (return ?x) 
) 

/*
** Asks users questions and takes in input to gather information. 
** Increments the question count by one every time a question is asked. 
** Checks question numbers by calling exceedMax method to make sure that
** the program does not exceed asking the maximum number of questions. 
*/
(deffunction askQ (?q) 
   (bind ?v (str-cat ?q "? ")) ; binds the question text to ?v 
   (bind ?out (screen-input (ask ?v)))   ; asks ?v and uses screen-input to validate the user input 
   (return ?out)                         ; returns screened input so it can be asserted as facts 
)

/*
** Screens input by first reducing all inputs values to lowercase. 
** 
** In the valid case - 
** if the input is quit or q, this function halts the program. 
** if the input is idk, y, or yes, this function makes it yes.
** if the input is n or no, then this function makes it no.
**
** In the invalid case - 
** if the input is none of the values mentioned above, this function
** prints out an error message and asks the user to enter input again
** according to instructions. It repeatedly asks for new input until
** the user puts something valid into the program. 
*/
(deffunction screen-input (?x) 
   (bind ?x (lowcase ?x))

   (if (or (eq ?x quit) (eq ?x q)) then                                                                ; input is valid cases 
      (printout t "You have asked to quit the program, so it has been terminated." crlf) 
      (halt)
    
    elif (or (eq ?x idk) (eq ?x y) (eq ?x yes)) then (bind ?x yes)

    elif (or (eq ?x n) (eq ?x no)) then (bind ?x no) 
    
    
    else                                                                                               ; input is invalid case 
       (while (not (or (eq ?x n) (eq ?x no) (eq ?x y) (eq ?x yes) (eq ?x idk) (eq ?x q) (eq ?x quit))) ; repeatedly asks for input
          (bind ?x (ask "Input invalid, please follow the instructions and try again: "))
       ) 

       (if (or (eq ?x quit) (eq ?x q)) then                                                            ; the new input is now valid
          (printout t "You have asked to quit the program, so it has been terminated." crlf) 
          (halt)

        elif (or (eq ?x idk) (eq ?x y) (eq ?x yes)) then (bind ?x yes)

        else (bind ?x no)                                                                               
       ) 
   ) 

   (return ?x) 
)

/********************************
** 
** Start of game user interface and directions, including an introduction
** about the program, domain of it, and directions/guidelines about input. 
**
** In addition to printing out basic messages, it dynamically creates
** all the backward chaining rules by passing all the global list variables 
** that contain all relevant traits into the parse-for-dynam function. 
**
** This rule has a high salience level, which means that it will fire
** as the first thing in the program. 
*/ 
(defrule startup "Starter of the program" 
   (declare (salience 100)) 
=>  
   (parse-for-dynam ?*WINDOW*)
   (parse-for-dynam ?*ROOF*) 
   (parse-for-dynam ?*HEIGHT*) 
   (parse-for-dynam ?*MATERIAL*) 
   (parse-for-dynam ?*EXTERIOR*) 
   (parse-for-dynam ?*SHAPE*) 
   (parse-for-dynam ?*OTHER*) 
   (parse-for-dynam ?*COLOR*) 

   (printout t crlf "This Expert System identifies the style of single-family houses!" crlf) 
   (printout t "Note: if at some point you are unsure about a term, visit the arch.html file" crlf 
               "in the folder to see if you can find an illustration on the simple glossary page." crlf)

   (printout t crlf "Enter yes/no, y/n, or idk to the questions asked, case insensitive." crlf)
   (printout t "If you would like to quit the game, enter quit or q." crlf)
)

/********************************
**
** Rule to end the program when all possible questions have
** been asked but no match of house styles have been found.
** 
** Calls the replay function to ask if the user wants to try again. 
** 
** This rule will go onto the agenda immediately, but
** because the salience level is so low, it will
** just sit there until there are no rules left to
** fire. Only then will it be examined, at which point
** no match can be found within the program. 
**/
(defrule no-solution "Ends the program when no match can be found"
   (declare (salience -100))
   (not (guessed))
=> 
   (printout t crlf "Sorry, the program found no matches." crlf)
   (replay)
)

/********************************
**
** Rule to capture when a guess has been made.
** When guessed is asserted this rule will fire, telling the
** user that a guess has been made, so the program ends. 
**
** Calls the replay function to ask if the user wants to try again. 
**/
/
(defrule guess-made "Halts the program when a guess is made"
   (guessed) 
=>
   (printout t crlf "The guess has been made!" crlf)
   (replay) 
)

/*
** Asks the user if they want to try again, validates the input 
** by using the screen-input function, and either runs the program 
** again or end it after printing a message teaching the users
** how to manually run the program again once they quit it. 
**
** In the input valid case - 
** if it is yes, this function resets and runs the program again
** if the input is no, this function ends a program and prints out 
** a message to let the users know that they can simply type 
** "(reset) (run)" into the terminal if they want to try again later.
** 
** In the input invalid case - 
** see documentation of the screen-input function 
*/ 
(deffunction replay ()
   (bind ?x (screen-input (ask "Do you want to try again: ")))

   (if (eq ?x yes) then (reset) (run)
    else 
       (printout t "Enter (reset) (run) if you want to try again later." crlf) 
       (halt)
   )
  
   (return)
) 

(run)
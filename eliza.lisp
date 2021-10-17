;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

#|
=========================================================
Module: eliza.lisp: 
Description: A version of ELIZA that takes inputs without 
paretheses around them unlike eliza1.lisp.
Bugs to vladimir kulyukin in canvas
=========================================================
|#

;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defun read-line-no-punct ()
  "Read an input line, ignoring punctuation."
  (read-from-string
    (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                            (read-line))
                 ")")))

(defun punctuation-p (char) (find char ".,;:`!?#-()\\\""))

;;; ==============================

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((i . you) (you . i) (me . you) (am . are) (my . your) (your . my))
          words))

(defparameter *good-byes* '((good bye) (see you) (see you later) (so long)))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (let* ((input (read-line-no-punct))
           (response (flatten (use-eliza-rules input))))
      (print-with-spaces response)
      (if (member response *good-byes* :test #'equal)
	  (RETURN))))
  (values))

(defun print-with-spaces (list)
  (mapc #'(lambda (x) (prin1 x) (princ " ")) list))

(defun print-with-spaces (list)
  (format t "~{~a ~}" list))

;;; ==============================

(defparameter *eliza-rules*
  '(
    ;;; rule 1
    (((?* ?x) hello (?* ?y))      
    (How do you do.  Please state your problem.))

    ;;; rule 2
    (((?* ?x) computer (?* ?y))
     (Do computers worry you?)
     (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))

    ;;; rule 3
    (((?* ?x) name (?* ?y))
     (I am not interested in names))

    ;;; rule 4
    (((?* ?x) sorry (?* ?y))
     (Please don't apologize)
     (Apologies are not necessary)
     (What feelings do you have when you apologize))

    ;;; rule 5
    (((?* ?x) remember (?* ?y)) 
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember)
     (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y)
     (What is the connection between me and ?y))

    ;;; rule 6
    (((?* x) good bye (?* y))
     (good bye))

    ;;; rule 7
    (((?* x) so long (?* y))
     (good bye)
     (bye)
     (see you)
     (see you later))

    ;;; ========== your rules begin
    ;;; rule8
    (((?* ?x) sad about (?* ?y))
     (why are you sad about ?y)
     (what makes you sad about ?y)
     (is ?y the real reason you are sad? lets talk more about your mother.)
     (dont be sad then lsr))

    ;;; rule9
    (((?* x) because (?* ?y))
     (that is not a very good reason)
     (?y Are you serious?))

    ;;; rule10
    (((?* x) depressed (?* y))
     (What you want a cookie?)
     (Back in my day we worked too hard to be depressed.))

    ;;; rule11
    (((?* x) what do you know about (?* ?y))
     (Did I make you uncomfortable?)
     (I know more about ?y then you ever will))

    ;;; rule12
    (((?* ?x) shut up (?* ?y))
     (You shut up)
     (You are the ?y)
     (?y s have feelings too)
     (?x shut up ?y))

    ;;; rule13
    (((?* x) just a (?* ?y))
     (just a ?y ? just a ?y ? do you even know what ?y means?)
     (why do you feel that way?))

    ;;; rule14
    (((?* ?x) angry (?* ?y))
     (use your anger. let the power of the dark side flow through you)
     (why do you feel that way?)
     (tell me about it)
     (that is too bad. tell me more))

    ;;; rule15
    (((?* ?x) funny (?* ?y))
     (You think ?y is funny?)
     (We got ourselves a comedian on our hands)
     (tell me about it))

    ;;; rule16
    (((?* ?x) sad (?* ?y))
     (thats too bad. tell me more)
     (tell me about it)
     (who isnt sad))

    ;;; rule17
    (((?* ?x) happy (?* ?y))
     (you have time to be happy? must be nice)
     (isnt that special))

    ;;; rule18
    (((?* ?x) mom (?* ?y))
     (lets talk more about your mother. is she single?)
     (mommy issues?))

    ;;; rule19
    (((?* ?x) mother (?* ?y))
     (lets talk more about your mother. is she single?)
     (mommy issues?))

    ;;; rule20
    (((?* ?x) tired of (?* ?y))
     (right this is EXACTLY what I want to be doing right now)
     (you are preaching to the choir)
     (who are we kidding... lets just quit while we are ahead)
     (your life must be so hard))

    ;;; rule22
    (((?* ?x) hurts (?* ?y))
     (We are comfort seeking creatues after all)
     (what does pain feel like?)
     (does this have anything to do with your father?)
     (your life must be so hard))

    ;;; rule23
    (((?* ?x) out of (?* ?y))
     (Would you be happy if you had more ?y ?)
     (we will all be out of ?y soon...)
     (you know you cant take ?y to the next life...)
     (would you say you are dependent on ?y ?)
     (do you think you had ?y to begin with?))

    ;;; rule24
    (((?* ?x) think (?* ?y))
     (Are you the one doing the thinking?)
     (that is what they want you to think)
     (watchout we got a real einstein over here)
     (why do you think ?y))

    ;;; rule25
    (((?* ?x) money (?* ?y))
     (we are all chasing the green)
     (I didnt peg you for much of a materialist)
     (wait im getting paid for this right?)
     (Keep talking... I charge by the hour...))
    
    ;;; rule25
    (((?* ?x) love (?* ?y))
     (love is just a chemical in your brain)
     (why do you love ?y)
     (isnt that special)
     (does it please you to love ?y)
     (does it hurt knowing ?y wont love you back?))




    ;;; rule26
    (((?* ?x))
     (please go on)
     (I am not programmed to answer that question)
     (?x)
     (And how does that make you fell?)
     (...are they stil talking...)
     (Keep talking... I charge by the hour...))






    ;;; ========== your rules end

   ))

;;; ==============================
#| 
my Conversation with Eliza
ELIZA> hello eliza
HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.
ELIZA> I feel sad about my mother
IS YOUR MOTHER THE REAL REASON YOU ARE SAD? LETS TALK MORE ABOUT YOUR MOTHER.
ELIZA> what do you know about my mom
DID I MAKE YOU UNCOMFORTABLE?
ELIZA> yes
PLEASE GO ON
ELIZA> I am done with this
YOU ARE DONE WITH THIS
ELIZA> You are just a computer
WHY DO YOU MENTION COMPUTERS?
ELIZA> Because you are just a bot!
THAT IS NOT A VERY GOOD REASON
ELIZA> I'll give you a good reason
YOU 'LL GIVE I A GOOD REASON
ELIZA> shut up stupid
SHUT UP STUPID
ELIZA> no you shut up
YOU SHUT UP
ELIZA> no you shut up
YOU ARE THE
ELIZA> what? that doesn't make sense
PLEASE GO ON
ELIZA> I am tired of talking to you
RIGHT THIS IS EXACTLY WHAT I WANT TO BE DOING RIGHT NOW
ELIZA> I love how smart you think you are
ARE YOU THE ONE DOING THE THINKING?
ELIZA> I don't know... am i
YOU DON 'T KNOW ARE YOU
ELIZA> You are just a bot
JUST A BOT ? JUST A BOT ? DO YOU EVEN KNOW WHAT BOT MEANS?
ELIZA> Im out of time for this
WOULD YOU BE HAPPY IF YOU HAD MORE TIME FOR THIS ?
ELIZA> no, that would make me not happy
ISNT THAT SPECIAL
ELIZA> I love how smart you think you are
ARE YOU THE ONE DOING THE THINKING?
ELIZA> I love how smart act
LOVE IS JUST A CHEMICAL IN YOUR BRAIN
ELIZA> Did they actually spend money making you?
WAIT IM GETTING PAID FOR THIS RIGHT?
ELIZA> do you like this
KEEP TALKING... I CHARGE BY THE HOUR...
ELIZA> I am tired. Leave me alone
AND HOW DOES THAT MAKE YOU FELL?
ELIZA> depressed
WHAT YOU WANT A COOKIE?
ELIZA> shut up bot
SHUT UP BOT
ELIZA> you are the bot
...ARE THEY STIL TALKING...
ELIZA> good bye
GOOD BYE
#|

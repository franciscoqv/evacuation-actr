; ########################## DEFAULTS ##########################

(defvar *response*)
(defvar *i*)
(defvar *opt1* "a")
(defvar *opt2* "b")
(defvar *threshold*)  ; retrieval threshold
(setq *threshold* 0)


; ########################## GENERIC FUNCTIONS ##########################

(defmethod rpm-window-key-event-handler
  ((win rpm-window) key)
  (setf *response* (string key))
  (clear-exp-window)
  (proc-display)
)

; Shows the results in the console
(defun show nil
  (format t "~%[~A, ~A, ~A, ~A]" *opt1* *opt2* *response* (mp-time))
)

; Writes the results into a file.
(defun write-result ()

  (with-open-file (str "C:\\Users\\panch\\Google Drive\\Proyectos\\GitHub\\evacuation-actr\\ACT-R\\outputs\\experiment-threshold.csv"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str "~%~A,~A,~A,~A,~A,~A" *i* *opt1* *opt2* *response* (mp-time) *threshold*)
  )
  ; The ~% means "new line"
)

(defun write-titles ()

  (with-open-file (str "C:\\Users\\panch\\Google Drive\\Proyectos\\GitHub\\evacuation-actr\\ACT-R\\outputs\\experiment-threshold.csv"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)

    (format str "id, opt1, opt2, response, time, threshold")
  )
  ; The ~% means "new line"
)

; ########################## RUN EXPERIMENT FUNCTIONS ##########################

; Retrieval threshold simulator
; Runs several experiments adjusting in each time the retrieval threshold value.
(defun run-threshold-experiment()
  (loop for *index* from 1 to 100 do
    (setq *threshold* (/ *index* 10))
    (run-experiment-decision)
  )
)


; Runs the experiment several times. In each cycle, the model is reset.
(defun run-experiment-decision()
  (write-titles)
  (loop for *i* from 1 to 100 do
    (run-experiment-decision-once)
  )
)

; Runs the experiment only once (i.e. one subject)
(defun run-experiment-decision-once ()

  ;All combinations of known doors
  (run-first-model "b" "d")(write-result) ; <- the "run-first-model" function includes a reset.
  (run-model "c" "d")(write-result)
  (run-model "c" "b")(write-result)
  (run-model "b" "a")(write-result)

  (run-model "d" "c")(write-result)
  (run-model "a" "c")(write-result)
  (run-model "c" "a")(write-result)
  (run-model "a" "d")(write-result)

  (run-model "d" "b")(write-result)
  (run-model "d" "a")(write-result)
  (run-model "b" "c")(write-result)
  (run-model "a" "b")(write-result)

  (run-model "a" "e")(write-result)
  (run-model "e" "b")(write-result)
  (run-model "e" "c")(write-result)
  (run-model "d" "e")(write-result)

  (run-model "f" "d")(write-result)
  (run-model "a" "f")(write-result)
  (run-model "f" "c")(write-result)
  (run-model "f" "b")(write-result)

  (run-model "f" "g")(write-result)
  (run-model "h" "i")(write-result)
  (run-model "j" "k")(write-result)
  (run-model "l" "m")(write-result)
)

; Resets the model, opens the window and runs the experiment.
; PARAMS: the two alternatives ("a", "b", "c", "d") the model has to choose from.
(defun run-first-model (option1 option2)
  (reset)

  ; Open the window.
  (let
    (
      (window
        ; To hide the window, use: ":visible nil"
        (open-exp-window "Decision between two options" :visible nil)
      )
    )

    ; Install the window so the model can "see" it.
    (install-device window)

    ; Run the model.
    (run-model option1 option2)
  )
  ;(show)
)

; Runs the decision-making model.
; PARAMS: the two alternatives ("a", "b", "c", "d") the model has to choose from.
(defun run-model (option1 option2)
  ; The screen is cleared and the agent is shown two options
  (clear-exp-window)
  (run 60) ; <- the number is the amount of time the model will run for.

  (add-text-to-exp-window :text option1 :x 50 :y 100)
  (add-text-to-exp-window :text option2 :x 100 :y 100)

  ; The response is set to nil
  (setf *response* nil)

  ; Set the options according to what has been received in the function
  (setf *opt1* option1)
  (setf *opt2* option2)

  ; The display is processed and the experiment is run for n seconds
  (proc-display)
  ;(run 2 :real-time t)
  (run 60)

  ; The screen is cleared
  ;(clear-exp-window)

  *response*
)

;------------------- MODEL -------------------;

(clear-all)

(define-model choice

  (eval `(sgp :rt, *threshold*))
  (sgp :v nil :esc t :show-focus t :esc t :ans 0.5)
  ;(sgp :esc t :show-focus t :esc t :ans 0.5)

  ; sgp   = set general parameter
  ; :v    = verbose
  ; :esc  = enable subsymbolic components
  ; :ul   = utility learning
  ; :egs  = utility noise parameter (production's utilities)
  ; :rt   = retrieval threshold
  ; :ans  = instantaneous noise
  ; :vwt  = virtual window trace

  ; CHUNK TYPES

  ; for the goal module
  (chunk-type goal state value)

  ; cue’s data vector
  (chunk-type data option cue value)

  ; imaginal module chunk
  (chunk-type evaluation option1 option2 value1 value2)

  (add-dm
    ;CHUNKS

    ; cues
    (cue1 ISA chunk) ;cue1
    (cue2 ISA chunk) ;cue2

    ; goal’s states
    (retrieve_cue ISA chunk)
    (process_cue ISA chunk)
    (start ISA chunk)
    (chosen ISA chunk)
    (perceive ISA chunk)
    (find ISA chunk)

    ; goal’s initial state
    (goal ISA goal state find value cue1)

    ; cue’s data vectors. This information is learned by the human agent.
    (cue1_a ISA data option "a" cue cue1 value 1)
    (cue2_a ISA data option "a" cue cue2 value 1)

    (cue1_b ISA data option "b" cue cue1 value 1)
    (cue2_b ISA data option "b" cue cue2 value 0)

    (cue1_c ISA data option "c" cue cue1 value 0)
    (cue2_c ISA data option "c" cue cue2 value 1)

    (cue1_d ISA data option "d" cue cue1 value 0)
    (cue2_d ISA data option "d" cue cue2 value 0)
  )


; START

  (P start
    =goal>
      state   start
    ?imaginal>
      buffer     empty
      state      free
  ==>
    !bind! =option1 *opt1*
    !bind! =option2 *opt2*

    +imaginal>
      isa evaluation
      ;option1 "a"
      option1 =option1
      option2 =option2
    =goal>
      state   retrieve_cue
  )



; RETRIEVE CUE

  (P retrieve_cue_data_option1
    =goal>
      state   retrieve_cue
      value   =cue
    ?imaginal>
      state      free
    =imaginal>
      option1 =option
      value1  nil
  ==>
    +retrieval>
      isa     data
      option  =option
      cue     =cue
    =goal>
      state   process_cue
    =imaginal>
      ;preventing harvest
  )

  (P retrieve_cue_data_option2
    =goal>
      state   retrieve_cue
      value   =cue
    ?imaginal>
      state   free
    =imaginal>
      option2 =option
      - value1  nil
      value2  nil
  ==>
    +retrieval>
      isa     data
      option  =option
      cue     =cue
    =goal>
      state   process_cue
    =imaginal>
      ;preventing harvest
  )



; SAVE CUE DATA

  (P save_cue_data_option1
    =goal>
      state   process_cue
      value   =cue
    =retrieval>
      isa     data
      option  =option
      cue     =cue
      value   =value
    ?imaginal>
      state      free
    =imaginal>
      value1  nil
  ==>
    =imaginal>
      value1 =value
    =goal>
      state retrieve_cue
  )

  (P save_cue_data_option1_error
    =goal>
      state   process_cue
      value  =cue
    ?retrieval>
      state   error
    ?imaginal>
      state   free
    =imaginal>
      value1  nil ; First alternative had an error
  ==>
    =imaginal>
      value1 "?"
    =goal>
      state retrieve_cue
  )

  (P save_cue_data_option2
    =goal>
      state   process_cue
      value   =cue
    =retrieval>
      isa     data
      option  =option
      cue     =cue
      value   =value
    ?imaginal>
      state      free
    =imaginal>
      - value1  nil
      value2  nil
  ==>
    =imaginal>
      value2 =value
    =goal>
      state retrieve_cue
  )

  (P save_cue_data_option2_error
    =goal>
      state   process_cue
      value  =cue
    ?retrieval>
      state   error
    ?imaginal>
      state   free
    =imaginal>
      - value1  nil
        value2  nil ; Second alternative had an error
  ==>
    =imaginal>
      value2 "?"
    =goal>
      state retrieve_cue
  )


  (P change_cue
    =goal>
      state   retrieve_cue
      value   cue1
    ?imaginal>
      state   free
    =imaginal>
      value1  =v
      value2  =v ;the two alternatives have the same value in the current cue
  ==>
    ;reset the evaluation
    =imaginal>
      value1  nil
      value2  nil
    ;next cue
    =goal>
      state   retrieve_cue
      value   cue2
  )


  ; DECISION

  (P choose_option_1
    =goal>
      state   retrieve_cue
    ?imaginal>
      state   free
    =imaginal>
      option1 =option
      value1  1
      value2  0
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )

  (P choose_option_2
    =goal>
      state   retrieve_cue
    ?imaginal>
      state   free
    =imaginal>
      option2 =option
      value1  0
      value2  1
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )


  ; ERROR  ; occurs when the cue cannot be retrieved

  (P choose_option_1_error_unknown_is_1
    =goal>
      state   retrieve_cue
      value   cue1  ; safety cue
    ?imaginal>
      state   free
    =imaginal>
      option1 =option
      value1  "?"
      value2  0
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )

  (P choose_option_1_error_unknown_is_2
    =goal>
      state   retrieve_cue
      value   cue1  ; safety cue
    ?imaginal>
      state   free
    =imaginal>
      option1 =option
      value1  1
      value2  "?"
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )

  (P choose_option_2_error_unknown_is_1
    =goal>
      state   retrieve_cue
      value   cue1  ; safety cue
    ?imaginal>
      state   free
    =imaginal>
      option2 =option
      value1  "?"
      value2  1
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )

  (P choose_option_2_error_unknown_is_2
    =goal>
      state   retrieve_cue
      value   cue1  ; safety cue
    ?imaginal>
      state   free
    =imaginal>
      option1 =option
      value1  0
      value2  "?"
  ==>
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )





  (P choose_randomly
    =goal>
      state   retrieve_cue
      value   cue1
    ?imaginal>
      state   free
    =imaginal>
      value1  "?"
      value2  "?"
  ==>
    ; decision is random
    !bind! =option (if (> (random 1.0) 0.5) *opt1* *opt2*)
    ; decision has been made
    =goal>
      state chosen
      value =option
    =imaginal>
  )







  ; Press the keys

  (P press-left-key
    =goal>
      state chosen
      value =option1
    =imaginal>
      option1 =option1
    ?manual>
      state free
  ==>
    +manual>
      cmd press-key
      key "f"
    =goal>
      ;state end
      state find
      value cue1
    -imaginal>
  )

  (P press-right-key
    =goal>
      state chosen
      value =option2
    =imaginal>
      option2 =option2
    ?manual>
      state free
  ==>
    +manual>
      cmd press-key
      key "j"
    =goal>
      ;state end
      state find
      value cue1
    -imaginal>
  )

  ;;; PERCEPTION

  (P find-unattended-message
    =goal>
      state find
    ?manual>
      state free
    ?visual>
      scene-change t
      state free
  ==>
    +visual-location>
      :attended nil
      screen-x lowest ; start from the left
    =goal>
      state attend
  )

  (P try-to-find
   =goal>
      state attend
    ?visual>
      state error
  ==>
    =goal>
      state find
    +visual>
      isa vision-command ; optional but safer
      cmd clear
  )

  (P attend-message
    =goal>
      state   attend
    =visual-location>; there has to be something in the visual-location buffer
    ?visual>
      state   free
  ==>
    +visual>
      cmd         move-attention
      screen-pos  =visual-location
    =goal>
      state   encode
  )

  (P encode-first-message
    =goal>
      state   encode
    =visual>
      value   =message
    ?imaginal>
      buffer  empty
      state   free
  ==>
    =goal>
      state   find
    +imaginal>
      isa     evaluation
      option1 =message
  )

  (P encode-second-message
    =goal>
      state   encode
    =visual>
      value   =message
    ?imaginal>
      state   free
    =imaginal>
      isa evaluation
    - option1 nil
  ==>
    =goal>
      state   retrieve_cue
    =imaginal>
      option2 =message
  )

  ;;;; END PERCEPTION


  ; Setting base-levels
  (set-base-levels (cue1_a 3.0))
  (set-base-levels (cue1_b 3.0))
  (set-base-levels (cue1_c 3.0))
  (set-base-levels (cue1_d 3.0))

  (set-base-levels (cue2_a 2.5))
  (set-base-levels (cue2_b 2.5))
  (set-base-levels (cue2_c 2.5))
  (set-base-levels (cue2_d 2.5))

  ; Goal

  (goal-focus goal)

)

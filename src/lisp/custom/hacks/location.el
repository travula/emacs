(defun guess-gateway ()
  "Guesses the gateway of the current network and returns it as a string"
  (interactive)
  (shell-command-to-string 
   ;; assumes bash shell
   (concat "route | grep --max-count=1 default | "
	   "tr -s \" \" \"|\" | cut -d \"|\" -f2")))

(defun guess-location ()
  "Guesses the  location of this machine.  
   Location is a symbolic name, currently one of iiitmk, home, or unknown"
  (interactive)
  (let ((gateway (guess-gateway)))
    (case (string-to-symbol gateway)
      ('godavari 'iiitmk)
      ('anant  'iiitmk)
      ('proxy  'iiitmk)
      (|192.168.1.1| 'home)
      (|202.88.232.1| 'asianet)
      (t 'unknown))))








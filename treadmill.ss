(import :gerbil/gambit
        :std/format
        :std/net/repl
        :std/sugar
        :std/srfi/1
        :thunknyc/apropos)

(export start-treadmill!
        eval-string/input-string)

(def (start-treadmill!)
  (let* ((s (start-repl-server! address: "127.0.0.1:0"))
         (port (socket-info-port-number
                (tcp-server-socket-info
                 (thread-specific s)))))
    (printf "Running net repl on port ~A.\n" port)
    (_gx#load-expander!)
    (thread-join! s)))

(def (eval/input e p)
  (let ((out (open-output-string))
        (err (open-output-string)))
    (parameterize ((current-input-port p)
                   (current-output-port out)
                   (current-error-port err))
      (let (result (call-with-values
                       (lambda ()
                         (try
                          (eval e)
                          (catch (e)
                            (eprintf "*** ERROR ~A ~A ~S\n"
                                     (error-message e)
                                     (error-trace e)
                                     (error-irritants e)))))
                     (lambda vals vals)))
        `(,result
          ,(get-output-string out)
          ,(get-output-string err))))))

(def (read-string s)
  (let (p (open-input-string s))
    (try
     (parameterize ((current-input-port p))
       (let lp ((vs '()) (val (read)))
         (if (eof-object? val) (reverse! vs)
             (lp (cons val vs) (read)))))
     (catch (e)
       (error "Error while reading -- check for an incomplete form.")))))

(def (eval-string/input-string e-s i-s)
  (try
   (let* ((exprs (read-string e-s))
          (input (open-input-string i-s)))
     (let (result-sets (map (cut eval/input <> input) exprs))
       (fold (lambda (result accum)
               (with (([rvals rout rerr] result)
                      ([avals aout aerr] accum))
                 (list (append avals rvals)
                       (string-append aout rout)
                       (string-append aerr rerr))))
             '(() "" "")
             result-sets)))
   (catch (e)
     '(() "" "*** ERROR EOF reached while reading\n"))))

#lang racket

(define (lexerAritmetico input)
  (define p1 (open-input-file input))
  (define lista (recorre p1))
  (define outputFile (open-output-file "output.html" #:exists 'replace))
  (displayln "<style> body {background-color: #23262e;} p {display: inline; color: white} .types {color: MediumOrchid;} .operators {color: red;} .numbers {color: orange;} .tabbed {padding: 0; margin: 0 0 0 25px;}</style>" outputFile)
  (escribir lista outputFile)
  (close-input-port p1)
  (close-output-port outputFile))

(define specialChars (flatten(list (bytes->list #";")
                                   (bytes->list #"{")
                                   (bytes->list #"}")
                                   (bytes->list #"(")
                                   (bytes->list #")")
                                   (bytes->list #"[")
                                   (bytes->list #"]")
                                   (bytes->list #" ")
                                   (bytes->list #"\n")
                                   (bytes->list #"\t"))))

(define dataTypes (list 'void 'float 'int 'double 'string 'char))
(define predefinedFuncs (list '("<p class='types'>if</p>") '("<p class='types'>for</p>") '("<p class='types'>while</p>") '("<p class='types'>else</p>")))

(define (convierte atomo p1 flag)
  (if flag
         (cond
           [(regexp-match? #rx";" (bytes atomo)) (list "<p class='semicolon'>;</p>")]
           [(regexp-match? #rx"{" (bytes atomo)) (list "<p class='brace'>{</p><div class='tabbed'>")]
           [(regexp-match? #rx"}" (bytes atomo)) (list "</div><p class='brace'>}</p>")]
           [(eq? 40 atomo) (list "<p class='parenthesis'>(</p>")]
           [(eq? 41 atomo) (list "<p class='parenthesis'>)</p>")]
           [(eq? 91 atomo) (list "<p class='bracket'>[</p>")]
           [(eq? 93 atomo) (list "<p class='bracket'>]</p>")]
           [(eq? 32 atomo) (list "<p>&nbsp;</p>")]
           [(eq? 10 atomo) (list "<br />")]
           [(eq? 9 atomo) (list "")])
         (cond
           [(integer? atomo) (list "<p class='numbers'>" atomo "</p>")]
           [(real? atomo) (list "<p class='numbers'>" atomo "</p>")]
           [(equal? '= atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '* atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '- atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '/ atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '^ atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '+ atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '== atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '< atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? '> atomo) (list "<p class='operators'>" atomo "</p>")]
           [(equal? 'while atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'for atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'if atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'else atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'int atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'double atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'float atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'string atomo) (list "<p class='types'>" atomo "</p>")]
           [(equal? 'char atomo) (list "<p class='types'>" atomo "</p>")] 
           [(equal? '// atomo) (list "Comentario: //" (read-line p1))]
           [(symbol? atomo) (list "<p class='variable'>" atomo "</p>")]
           [(list? atomo) atomo]
           [else (list "Atomo: " atomo)])))

(define (recorre p1)
  (cond
    [(eof-object? (peek-char p1)) '()]
    [(member (peek-byte p1) specialChars) (append (list(convierte (read-byte p1) p1 #t)) (recorre p1))]
    [else (append (list(convierte (read p1) p1 #f)) (recorre p1))]))

(define (writeFunc file list)
  (map (lambda (item)
         (display item file)) list)
  (displayln "" file))

(define (escribir lista file)
  (map (lambda (i)
           (writeFunc file i))
       lista))
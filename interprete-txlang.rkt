#lang eopl
;Autores: Aura Maria Pelaez Luna,202459422-Valentina Valencia Lopez,202459626-Juan Felipe Aristízabal,2459364

#|
  Interpretador de TxLang-UV.
  Proyecto final del curso Fundamentos de Interpretación y Compilación de
  Lenguajes de Programación, Universidad del Valle.

  El archivo va en nueve secciones, en el orden en que se construye un
  interpretador. La primera entrega llena las secciones 1 a 6 y la 8; la
  segunda agrega al almacén el registro que permite revertir, los registros
  del lenguaje y el chequeador de tipos de la sección 7.

  Lo que hay ahora es el andamiaje mínimo para que las baterías corran: una
  gramática con un solo literal, un value-of de una cláusula y la interfaz
  que usan pruebas-txlang.rkt y ejemplos-anexo.rkt. Todo lo demás lo pone el
  grupo, siguiendo la gramática y la semántica del enunciado.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los dos tipos de comentario, los literales de cada clase y los
;; identificadores. El comentario de bloque no admite | adentro.

(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario-linea ("//" (arbno (not #\newline))) skip)
    (comentario-bloque ("#|" (arbno (not #\|)) "|#") skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" any "'") string)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva. Los nombres de las variantes los escoge el grupo.

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{"
      (separated-list identificador ":" tipo ",") "}")
     a-record)

    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) registro-tipo)

    (expresion (numero) lit-exp)
    (expresion (flotante) flotante-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) caracter-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)
    (expresion ("let" (separated-list identificador "=" expresion ",")
                "in" expresion "end") let-exp)
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expresion "then" expresion
                (arbno "elif" expresion "then" expresion)
                "else" expresion "end") if-exp)
    (expresion ("while" expresion "do" expresion "end") while-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion ("proc" "(" (separated-list identificador ":" tipo ",") ")"
                expresion "end") proc-exp)
    (expresion ("apply" expresion "(" (separated-list expresion ",") ")")
               apply-exp)
    (expresion ("letrec"
                (arbno tipo identificador
                       "(" (separated-list identificador ":" tipo ",") ")"
                       "=" expresion)
                "in" expresion "end") letrec-exp)
    (expresion ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
               atomic-exp)
    (expresion ("abort" "(" expresion ")") abort-exp)
    (expresion ("new" identificador "{"
                (separated-list identificador "=" expresion ",") "}") new-exp)
    (expresion ("field" "(" expresion "," identificador ")") field-exp)

    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================

;; une una lista de textos poniendo un separador entre cada uno
(define unir
  (lambda (lista separador)
    (cond ((null? lista) "")((null? (cdr lista)) (car lista))
          (else (string-append (car lista) separador(unir (cdr lista) separador))))))

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (string-append
         (apply string-append
                (map (lambda (d) (string-append (unparse-decl d) "\n")) decls))
         (unparse-expresion cuerpo))))))

(define unparse-decl
  (lambda (decl)
    (cases decl-registro decl
      (a-record (nombre campos tipos)
        (string-append "record " (symbol->string nombre) " { "(unparse-params campos tipos) " }")))))

;; arma "x: int, y: bool"
(define unparse-params
  (lambda (ids tipos)
    (unir (map (lambda (id un-tipo)
                 (string-append (symbol->string id) ": " (unparse-tipo un-tipo)))ids tipos)", ")))

(define unparse-tipo
  (lambda (un-tipo)
    (cases tipo un-tipo
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t) (string-append "ref(" (unparse-tipo t) ")"))
      (proc-tipo (tipos-args tipo-ret)
        (string-append "(" (unir (map unparse-tipo tipos-args) ", ")" -> " (unparse-tipo tipo-ret) ")"))
      (registro-tipo (id) (symbol->string id)))))

(define unparse-prim
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () "+") (resta-prim () "-") (mult-prim () "*")
      (div-prim () "/") (mod-prim () "%")
      (mayor-prim () ">") (menor-prim () "<")
      (mayor-igual-prim () ">=") (menor-igual-prim () "<=")
      (igual-prim () "==") (diferente-prim () "!=")
      (and-prim () "and") (or-prim () "or") (not-prim () "not")
      (concat-prim () "concat") (length-prim () "length")
      (to-string-prim () "to-string") (to-number-prim () "to-number")
      (print-prim () "print"))))

;; arma "a = e1, b = e2" (sirve para let y para new)
(define unparse-ligaduras
  (lambda (ids exps)
    (unir (map (lambda (id e)
                 (string-append (symbol->string id) " = " (unparse-expresion e)))ids exps)", ")))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n))
      (flotante-exp (f) (number->string f))
      (cadena-exp (c) c)
      (caracter-exp (c) c)
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))
      (let-exp (ids exps body)
        (string-append "let " (unparse-ligaduras ids exps)" in " (unparse-expresion body) " end"))
      (ref-exp (e) (string-append "ref(" (unparse-expresion e) ")"))
      (deref-exp (e) (string-append "deref(" (unparse-expresion e) ")"))
      (set-exp (e1 e2)
        (string-append "set " (unparse-expresion e1) " := " (unparse-expresion e2)))
      (begin-exp (primera resto)
        (string-append "begin "(unir (map unparse-expresion (cons primera resto)) "; ")" end"))
      (if-exp (prueba entonces pruebas-elif entonces-elif sino)
        (string-append
         "if " (unparse-expresion prueba) " then " (unparse-expresion entonces)
         (apply string-append
                (map (lambda (p r)
                       (string-append " elif " (unparse-expresion p)" then " (unparse-expresion r)))
                     pruebas-elif entonces-elif))" else " (unparse-expresion sino) " end"))
      (while-exp (condicion body)
        (string-append "while " (unparse-expresion condicion)" do " (unparse-expresion body) " end"))
      (prim-exp (prim args)
        (string-append (unparse-prim prim)"(" (unir (map unparse-expresion args) ", ") ")"))
      (proc-exp (ids tipos body)
        (string-append "proc (" (unparse-params ids tipos) ") "(unparse-expresion body) " end"))
      (apply-exp (rator rands)
        (string-append "apply " (unparse-expresion rator)"(" (unir (map unparse-expresion rands) ", ") ")"))
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos body)
        (string-append
         "letrec "
         (unir (map (lambda (tr nombre ids tipos c)
                      (string-append (unparse-tipo tr) " " (symbol->string nombre)
                                     "(" (unparse-params ids tipos) ") = "
                                     (unparse-expresion c)))
                    tipos-ret nombres idss tiposs cuerpos)
               " ")
         " in " (unparse-expresion body) " end"))
      (atomic-exp (body id si-falla)
        (string-append "atomic " (unparse-expresion body)" onfail (" (symbol->string id) ") "
                       (unparse-expresion si-falla) " end"))
      (abort-exp (e) (string-append "abort(" (unparse-expresion e) ")"))
      (new-exp (nombre ids exps)
        (string-append "new " (symbol->string nombre) " { "
                       (unparse-ligaduras ids exps) " }"))
      (field-exp (e id)
        (string-append "field(" (unparse-expresion e) ", " (symbol->string id) ")")))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (lids (list-of symbol?))
   (lvalue list?)
   (old-env ambiente?))
  ;; marco de un letrec: guarda los ingredientes de las clausuras, no las clausuras
  (ambiente-extendido-recursivo
   (procnames (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (old-env ambiente?)))

(define apply-env
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "No se encuentra ~s" var))
      (ambiente-extendido (lids lvalue old-env)
        (letrec
            ((buscar-variable
              (lambda (lids lvalue)
                (cond
                  ((null? lids) (apply-env old-env var))((equal? (car lids) var) (car lvalue))
                  (else (buscar-variable (cdr lids) (cdr lvalue)))))))
          (buscar-variable lids lvalue)))
      (ambiente-extendido-recursivo (procnames idss cuerpos old-env)
        (letrec
            ((buscar-variable
              (lambda (procnames idss cuerpos)
                (cond
                  ((null? procnames) (apply-env old-env var))
                  ;; la clausura se arma con env (el marco recursivo), no con old-env
                  ((equal? (car procnames) var)
                   (closure (car idss) (car cuerpos) env))
                  (else (buscar-variable (cdr procnames) (cdr idss) (cdr cuerpos)))))))
          (buscar-variable procnames idss cuerpos))))))

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================
;; Celdas que se reservan con ref, se leen con deref y se escriben con set.
;; En la primera entrega las escrituras son definitivas; en la segunda el
;; almacén lleva el registro que permite revertir un bloque atomic. Conviene
;; diseñar la interfaz desde ahora pensando en eso.
;; una referencia es la posicion de una celda en el almacen
(define-datatype referencia referencia?
  (a-ref (pos integer?)))

;; el almacen es una lista: la posicion de cada valor es su referencia
(define el-almacen 'sin-inicializar)

(define initialize-store!
  (lambda ()
    (set! el-almacen '())))

;; newref: agrega una celda al final y devuelve su referencia
(define newref
  (lambda (valor)
    (let ((siguiente (length el-almacen)))
      (begin
        (set! el-almacen (append el-almacen (list valor)))
        (a-ref siguiente)))))

;; deref: lee el contenido de la celda
(define deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos)
        (if (< pos (length el-almacen))
            (list-ref el-almacen pos)
            (eopl:error 'deref "La referencia ~s no apunta a ninguna celda del almacen" pos))))))

;; setref!: escribe en la celda
;; (en la entrega 2 aqui se guarda el valor anterior para las transacciones)
(define setref!
  (lambda (ref valor)
    (cases referencia ref
      (a-ref (pos)
        (set! el-almacen (reemplazar-celda el-almacen pos valor))))))

;; arma una lista igual a la de celdas pero con valor en la posicion pos
(define reemplazar-celda
  (lambda (celdas pos valor)
    (cond
      ((null? celdas)
       (eopl:error 'setref! "La referencia no apunta a ninguna celda del almacen"))
      ((= pos 0) (cons valor (cdr celdas)))
      (else (cons (car celdas) (reemplazar-celda (cdr celdas) (- pos 1) valor))))))

;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

;; el valor unit lo represento con un simbolo
(define valor-unit 'unit)

;; una clausura guarda parametros, cuerpo y el ambiente donde se creo
(define-datatype procval procval?
  (closure (ids list?) (body expresion?) (env ambiente?)))

;; texto con el que correr muestra un valor
(define valor->texto
  (lambda (v)
    (cond ((number? v) (number->string v))
          ((string? v) (string-append "\"" v "\""))
          ((char? v) (string-append "'" (string v) "'"))
          ((boolean? v) (if v "true" "false"))
          ((eq? v valor-unit) "unit")
          ((referencia? v) "<ref>")
          ((procval? v) "<proc>")
          (else "<valor>"))))

;; texto sin comillas (lo usan print y to-string)
(define valor->cadena
  (lambda (v)
    (cond ((string? v) v)
          ((char? v) (string v))
          (else (valor->texto v)))))

;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================
;; Recibe el programa y lo devuelve si está bien tipado; si no, aborta con
;; eopl:error antes de que se evalúe. Mientras no exista, deja pasar todo.

(define chequear-programa
  (lambda (pgm) pgm))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))



;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================

(define evaluar-programa
  (lambda (pgm)
    (begin
      (initialize-store!)
      (cases programa pgm
        (un-programa (decls cuerpo)
          (value-of cuerpo (ambiente-vacio)))))))

;; las cadenas y caracteres vienen con comillas desde el scanner
(define quitar-comillas
  (lambda (texto)
    (substring texto 1 (- (string-length texto) 1))))

;; quien es solo para el mensaje de error (deref o setref)
(define como-referencia
  (lambda (valor quien)
    (if (referencia? valor)
        valor
        (eopl:error quien "Se esperaba una referencia y se obtuvo: ~s" valor))))

(define exigir-booleano
  (lambda (valor)
    (if (boolean? valor)
        valor
        (eopl:error 'if "La prueba debe ser booleana y se obtuvo: ~s" valor))))

;; evalua una lista de expresiones de izquierda a derecha
(define evaluar-lista
  (lambda (exps amb)
    (map (lambda (e) (value-of e amb)) exps)))

;; begin: evalua todas y devuelve la ultima
(define evaluar-secuencia
  (lambda (primera resto amb)
    (let ((valor (value-of primera amb)))
      (if (null? resto)
          valor
          (evaluar-secuencia (car resto) (cdr resto) amb)))))

;; if con sus elif: va probando una prueba tras otra
(define evaluar-if
  (lambda (prueba entonces pruebas-elif entonces-elif sino amb)
    (if (exigir-booleano (value-of prueba amb))
        (value-of entonces amb)
        (if (null? pruebas-elif)
            (value-of sino amb)
            (evaluar-if (car pruebas-elif) (car entonces-elif)
                        (cdr pruebas-elif) (cdr entonces-elif)
                        sino amb)))))

(define aplicar-procedimiento
  (lambda (proc args)
    (if (procval? proc)
        (cases procval proc
          (closure (ids body env)
            (if (= (length ids) (length args))
                (value-of body (ambiente-extendido ids args env))
                (eopl:error 'apply "Se esperaban ~s argumentos y llegaron ~s"
                            (length ids) (length args)))))
        (eopl:error 'apply "Se esperaba un procedimiento y se obtuvo: ~s" proc))))

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) n)
      (flotante-exp (f) f)
      (cadena-exp (c) (quitar-comillas c))
      (caracter-exp (c) (string-ref c 1))
      (true-exp () #t)
      (false-exp () #f)
      (unit-exp () valor-unit)
      (var-exp (id) (apply-env amb id))
      ;; las ligaduras se evaluan en el ambiente de afuera
      (let-exp (ids exps body)
        (value-of body (ambiente-extendido ids (evaluar-lista exps amb) amb)))
      (ref-exp (e) (newref (value-of e amb)))
      (deref-exp (e)
        (deref (como-referencia (value-of e amb) 'deref)))
      (set-exp (e1 e2)
        (let ((ref (como-referencia (value-of e1 amb) 'setref)))
          (let ((valor (value-of e2 amb)))
            (begin (setref! ref valor)
                   valor-unit))))
      (begin-exp (primera resto) (evaluar-secuencia primera resto amb))
      (if-exp (prueba entonces pruebas-elif entonces-elif sino)
        (evaluar-if prueba entonces pruebas-elif entonces-elif sino amb))
      (while-exp (condicion body)
        (letrec ((ciclo (lambda ()
                          (if (exigir-booleano (value-of condicion amb))(begin (value-of body amb) (ciclo))
                              valor-unit))))
          (ciclo)))
      (prim-exp (prim args)
        (evaluar-primitiva prim (evaluar-lista args amb)))
      (proc-exp (ids tipos body) (closure ids body amb))
      (apply-exp (rator rands)
        (let ((proc (value-of rator amb)))
          (aplicar-procedimiento proc (evaluar-lista rands amb))))
      (letrec-exp (tipos-ret procnames idss tiposs cuerpos body)
        (value-of body (ambiente-extendido-recursivo procnames idss cuerpos amb)))
      ;; atomic, abort, new y field son de la segunda entrega
      (else (eopl:error 'value-of
                        "Esta construccion se implementa en la segunda entrega")))))

;; ---------- primitivas ----------

;; revisa que lleguen dos numeros y devuelve la lista de argumentos
(define dos-numeros
  (lambda (nombre args)
    (if (and (= (length args) 2)
             (number? (car args))
             (number? (cadr args)))
        args
        (eopl:error nombre "Se esperaban dos numeros y llegaron: ~s" args))))

(define dos-booleanos
  (lambda (nombre args)
    (if (and (= (length args) 2)
             (boolean? (car args))
             (boolean? (cadr args)))
        args
        (eopl:error nombre "Se esperaban dos booleanos y llegaron: ~s" args))))

(define un-argumento
  (lambda (nombre args)
    (if (= (length args) 1)
        (car args)
        (eopl:error nombre "Se esperaba un solo argumento y llegaron: ~s" args))))

(define todas-cadenas?
  (lambda (lista)
    (cond ((null? lista) #t)
          ((string? (car lista)) (todas-cadenas? (cdr lista)))
          (else #f))))

;; los numeros se comparan con = y lo demas con equal?
(define son-iguales?
  (lambda (a b)
    (if (and (number? a) (number? b))
        (= a b)
        (equal? a b))))

;; evaluar-primitiva: Primitiva x Lista(Valor) -> Valor
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (suma-prim () (apply + (dos-numeros '+ args)))
      (resta-prim () (apply - (dos-numeros '- args)))
      (mult-prim () (apply * (dos-numeros '* args)))
      ;; entre enteros / es division entera
      (div-prim ()
        (let ((a (car (dos-numeros '/ args)))
              (b (cadr args)))
          (cond ((zero? b) (eopl:error '/ "Division por cero"))
                ((and (exact? a) (exact? b)) (quotient a b))
                (else (/ a b)))))
      ;; % solo acepta enteros
      (mod-prim ()
        (let ((a (car (dos-numeros '% args)))
              (b (cadr args)))
          (cond ((not (and (exact? a) (exact? b)))
                 (eopl:error '% "% solo acepta enteros"))
                ((zero? b) (eopl:error '% "Division por cero"))
                (else (modulo a b)))))
      (mayor-prim () (apply > (dos-numeros '> args)))
      (menor-prim () (apply < (dos-numeros '< args)))
      (mayor-igual-prim () (apply >= (dos-numeros '>= args)))
      (menor-igual-prim () (apply <= (dos-numeros '<= args)))
      (igual-prim ()
        (if (= (length args) 2)
            (son-iguales? (car args) (cadr args))
            (eopl:error '== "Se esperaban dos argumentos")))
      (diferente-prim ()
        (if (= (length args) 2)
            (not (son-iguales? (car args) (cadr args)))
            (eopl:error '!= "Se esperaban dos argumentos")))
      (and-prim ()
        (let ((bools (dos-booleanos 'and args)))
          (and (car bools) (cadr bools))))
      (or-prim ()
        (let ((bools (dos-booleanos 'or args)))
          (or (car bools) (cadr bools))))
      (not-prim ()
        (let ((b (un-argumento 'not args)))
          (if (boolean? b)
              (not b)
              (eopl:error 'not "Se esperaba un booleano: ~s" b))))
      (concat-prim ()
        (if (todas-cadenas? args)
            (apply string-append args)
            (eopl:error 'concat "concat exige cadenas: ~s" args)))
      (length-prim ()
        (let ((c (un-argumento 'length args)))
          (if (string? c)
              (string-length c)
              (eopl:error 'length "Se esperaba una cadena: ~s" c))))
      (to-string-prim ()
        (valor->cadena (un-argumento 'to-string args)))
      (to-number-prim ()
        (let ((c (un-argumento 'to-number args)))
          (if (and (string? c) (string->number c))
              (string->number c)
              (eopl:error 'to-number "No se puede convertir a numero: ~s" c))))
      ;; print escribe en consola y devuelve unit
      (print-prim ()
        (begin (display (valor->cadena (un-argumento 'print args)))
               (newline)
               valor-unit)))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (evaluar-programa pgm)))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))
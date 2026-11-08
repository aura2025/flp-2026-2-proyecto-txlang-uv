#lang eopl
;Autores: Aura Maria Pelaez Luna,202459422-Valentina Valencia Lopez,202459626-Juan Felipe Aristízabal,2459364
#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")

;; Agregue aquí las pruebas de la primera entrega: literales de cada clase,
;; let, ref/deref/set, begin, if con elif, while, cada primitiva, proc y
;; apply, letrec con recursión mutua. Por ejemplo:
;;
;; (prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
;; (prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
;; (prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
;; literales
(prueba "entero negativo" "-7" "-7")
(prueba "flotante" "3.14" "3.14")
(prueba "cadena" "\"hola\"" "\"hola\"")
(prueba "caracter" "'a'" "'a'")
(prueba "booleano" "true" "true")
(prueba "unit" "unit" "unit")
;; comentarios
(prueba "comentario de linea" "// hola\n+(1, 2)" "3")
(prueba "comentario de bloque" "#| un bloque |# +(1, 2)" "3")
;; primitivas
(prueba "aritmetica" "*(-(10, 4), 2)" "12")
(prueba "division entera" "/(7, 2)" "3")
(prueba "division flotante" "/(7.0, 2.0)" "3.5")
(prueba "modulo" "%(7, 2)" "1")
(prueba "relacional" "<(1, 2)" "true")
(prueba "mayor o igual" ">=(2, 2)" "true")
(prueba "diferente" "!=(1, 2)" "true")
(prueba "igualdad de cadenas" "==(\"a\", \"a\")" "true")
(prueba "booleanas" "and(true, not(false))" "true")
(prueba "or" "or(false, false)" "false")
(prueba "concat" "concat(\"ab\", \"cd\")" "\"abcd\"")
(prueba "length" "length(\"hola\")" "4")
(prueba "to-string" "to-string(42)" "\"42\"")
(prueba "to-number" "to-number(concat(\"1\", \"7\"))" "17")
;; let, if, while, begin
(prueba "let" "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "let: las ligaduras no se ven entre si" "let x = 1 in let x = 2, y = x in y end end" "1")
(prueba "if con elif" "if false then 1 elif true then 2 else 3 end" "2")
(prueba "begin" "begin 1; 2; 3 end" "3")
(prueba "while"
        "let c = ref(0) in begin while <(deref(c), 5) do set c := +(deref(c), 1) end; deref(c) end end"
        "5")
(prueba "print devuelve unit" "begin print(\"hola\"); 5 end" "5")
;; almacen
(prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
(prueba "anexo 1"
        "let c = ref(0) in begin set c := +(deref(c), 1); set c := +(deref(c), 1); deref(c) end end"
        "2")
(prueba "anexo 7: referencia como argumento"
        "let celda = ref(10), duplicar = proc (r: ref(int)) set r := *(deref(r), 2) end in begin apply duplicar (celda); apply duplicar (celda); deref(celda) end end"
        "40")
;; procedimientos y letrec
(prueba "proc y apply" "let suma = proc (a: int, b: int) +(a, b) end in apply suma (3, 4) end" "7")
(prueba "alcance lexico"
        "let x = 10 in let f = proc (y: int) +(x, y) end in let x = 100 in apply f (1) end end end"
        "11")
(prueba "anexo 6: letrec"
        "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end in apply fact (5) end"
        "120")
(prueba "recursion mutua"
        "letrec bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end in apply par (10) end"
        "true")
;; errores
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba-rechazo "deref de un no referencia" "deref(5)")
(prueba-rechazo "set sobre un no referencia" "set 5 := 3")
(prueba-rechazo "division por cero" "/(1, 0)")
(prueba-rechazo "variable no ligada" "x")
(prueba-rechazo "apply de un no procedimiento" "apply 5 (1)")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")

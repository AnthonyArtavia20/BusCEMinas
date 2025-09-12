#lang racket

;;; DEFINICIÓN DE ESTRUCTURAS DE DATOS, es decir, como se va a representar el tablero.

;; Una celda es: (list mina? minas-adyacentes descubierta? bandera?)
;; donde: 
;; - mina?: boolean (#t true: tiene mina o #f false: No tiene mina)
;; - minas-adyacentes: número (0-8) que indica todo el alrededor de la celda seleccionada
;; - descubierta?: boolean (#t si la celda fue revelada, #f si está oculta osea no ha sido clickeada)
;; - bandera?: boolean (#t tiene bandera, #f no tiene)

;; Ejemplo: (list #f 3 #t #f) → No es mina, 3 minas adyacentes, descubierta, sin bandera

;; Un tablero es: (list (list celda) (list celda) ...), como una matriz, lista de lista. Matriz de filas x columnas donde cada elemento es una celda.

;;; FUNCIONES BÁSICAS PARA MANIPULAR CELDAS

(define (crear-celda-vacia)
  "Crea una celda vacía sin mina"
  (list #f 0 #f #f))

(define (crear-celda-mina)
  "Crea una celda con mina"
  (list #t 0 #f #f))
;;;-> Consulta de estado de cada celda.
(define (es-mina? celda)
  "Verifica si la celda tiene mina"
  (first celda))

(define (obtener-minas-adyacentes celda)
  "Obtiene el número de minas adyacentes"
  (second celda))

(define (esta-descubierta? celda)
  "Verifica si la celda está descubierta"
  (third celda))

(define (tiene-bandera? celda)
  "Verifica si la celda tiene bandera"
  (fourth celda))
;;;-> Modificación de celdas
(define (poner-mina celda)
  "Convierte una celda en mina, manteniendo sus otros atributos"
  (list #t (obtener-minas-adyacentes celda) 
        (esta-descubierta? celda) 
        (tiene-bandera? celda)))

(define (poner-minas-adyacentes celda numero)
  "Establece el número de minas adyacentes de una celda"
  (list (es-mina? celda) numero 
        (esta-descubierta? celda) 
        (tiene-bandera? celda)))

(define (descubrir-celda-func celda)
  "Marca la celda como descubierta (tercer elemento = #t)"
  (list (es-mina? celda) 
        (obtener-minas-adyacentes celda) 
        #t 
        (tiene-bandera? celda)))

(define (poner-bandera celda)
  "Alterna Pone/quita bandera en la celda"
  (list (es-mina? celda) 
        (obtener-minas-adyacentes celda) 
        (esta-descubierta? celda) 
        (not (tiene-bandera? celda))))

;;; ---> FUNCIONES PARA MANIPULAR TABLEROS

(define (crear-tablero-vacio filas columnas)  ;;;Función principal recursiva para la creación del tablero
  "Crea un tablero vacío de filas x columnas usando recursividad"
  (crear-tablero-vacio-aux filas columnas '()))

(define (crear-tablero-vacio-aux filas columnas resultado) ;;;Función auxiliar para la creación del tablero
  "Función auxiliar recursiva que construye el tablero fila por fila"
  (if (= filas 0)
      resultado ;caso base: retorna el tablero completo
      (crear-tablero-vacio-aux (- filas 1) columnas 
                               (cons (crear-fila-vacia columnas) resultado))))

;;;----------------------Creción de columnas y filas(Start)------------------------------------

(define (crear-fila-vacia columnas)
  "Crea una fila vacía con 'columnas' número de celdas"
  (crear-fila-vacia-aux columnas '()))

(define (crear-fila-vacia-aux columnas resultado)
  "Función auxiliar recursiva que construye una fila celda por celda"
  (if (= columnas 0)
      resultado ;Caso base, retorna la fila completa
      (crear-fila-vacia-aux (- columnas 1) 
                           (cons (crear-celda-vacia) resultado))))
;;;----------------------Creción de columnas filas(End)------------------------------------

;;;----------------------Para poder seleccionar una celda en el tablero(Start)------------------------------------
(define (obtener-celda tablero fila columna)
  "Obtiene la celda en la posición (fila, columna) del tablero(matriz)"
  (obtener-elemento (obtener-elemento tablero fila) columna))

(define (obtener-elemento lista indice)
  "Obtiene el elemento en el índice específico de una lista (recursivo)"
  (if (= indice 0)
      (first lista) ;Caso base, elemento encontrado
      (obtener-elemento (rest lista) (- indice 1)))); Llamada recursiva
;;;----------------------Para poder seleccionar una celda en el tablero(End)------------------------------------

;;;----------------------Actualización de celdas(Start)------------------------------------

(define (actualizar-celda tablero fila columna nueva-celda)
  "Actualiza una celda específica en el tablero con nuevos valores"
  (actualizar-fila tablero fila 
                  (actualizar-elemento (obtener-elemento tablero fila) 
                                      columna nueva-celda)))

(define (actualizar-fila tablero indice-fila nueva-fila)
  "Actualiza una fila específica en el tablero"
  (actualizar-elemento tablero indice-fila nueva-fila))

(define (actualizar-elemento lista indice nuevo-elemento)
  "Actualiza un elemento en una lista en la posición específica"
  (if (= indice 0)
      (cons nuevo-elemento (rest lista)) ;Reemplaza el elemento
      (cons (first lista) 
            (actualizar-elemento (rest lista) (- indice 1) nuevo-elemento))))
;;;----------------------Actualización de celdas(End)------------------------------------

;;; ><><><><><><><><><><><><><><><><>Funciones principales<><><><><><<><><><>><><><><><><><><><>><<
;;; FUNCIÓN PRINCIPAL PARA CREAR TABLERO CON MINAS

(define (crear-tablero-con-minas filas columnas porcentaje-minas)
  "Función principal: Crea un tablero con minas aleatorias según el porcentaje dado"
  (crear-tablero-con-minas-aux filas columnas porcentaje-minas (crear-tablero-vacio filas columnas)))

(define (crear-tablero-con-minas-aux filas columnas porcentaje-minas tablero)
  "Función auxiliar recursiva para crear tablero con minas"
  (define total-minas (exact-round (* filas columnas porcentaje-minas)))
  (colocar-minas-aleatorias tablero total-minas filas columnas))

(define (colocar-minas-aleatorias tablero minas-restantes filas columnas)
  "Coloca minas aleatorias en el tablero de forma recursiva"
  (if (= minas-restantes 0)
      (calcular-minas-adyacentes tablero filas columnas) ;Todas las minas colocadas
      (colocar-minas-aleatorias 
       (poner-mina-en-posicion-aleatoria tablero filas columnas)
       (- minas-restantes 1)
       filas columnas)))

(define (poner-mina-en-posicion-aleatoria tablero filas columnas)
  "Pone una mina en una posición aleatoria que no tenga mina"
  (poner-mina-en-posicion-aleatoria-aux tablero 
                                       (random filas) 
                                       (random columnas) 
                                       filas columnas))

(define (poner-mina-en-posicion-aleatoria-aux tablero fila columna filas columnas)
  "Auxiliar recursivo para poner mina en posición aleatoria"
  (if (es-mina? (obtener-celda tablero fila columna))
      (poner-mina-en-posicion-aleatoria tablero filas columnas) ;Posición ocupada, reintentar
      (actualizar-celda tablero fila columna 
                       (poner-mina (obtener-celda tablero fila columna)))));Colocar mina justo acá.

;;; <><>><><><><><><>><>>FUNCIÓN PARA CALCULAR MINAS ADYACENTES(Start)<><><><<><><><><><><><><><><>

(define (calcular-minas-adyacentes tablero filas columnas)
  "Calcula minas adyacentes para cada celda del tablero"
  (calcular-minas-adyacentes-fila tablero 0 filas columnas))

(define (calcular-minas-adyacentes-fila tablero fila-actual filas columnas)
  "Procesa cada fila del tablero para calcular minas adyacentes"
  (if (= fila-actual filas)
      tablero ;Caso base, todas las filas procesadas
      (calcular-minas-adyacentes-fila 
       (calcular-minas-adyacentes-columna tablero fila-actual 0 filas columnas)
       (+ fila-actual 1)
       filas columnas)))

(define (calcular-minas-adyacentes-columna tablero fila columna-actual filas columnas)
  "Calcula minas adyacentes para cada columna"
  (if (= columna-actual columnas)
      tablero ;Caso base, todas las columnas procesadas, listop
      (calcular-minas-adyacentes-columna 
       (actualizar-celda tablero fila columna-actual
                        (poner-minas-adyacentes 
                         (obtener-celda tablero fila columna-actual)
                         (contar-minas-cercanas tablero fila columna-actual filas columnas)))
       fila
       (+ columna-actual 1)
       filas columnas)))

(define (contar-minas-cercanas tablero fila columna filas columnas)
  "Cuenta minas en las 8 celdas adyacentes"
  (contar-minas-cercanas-aux tablero 
                            (list (- fila 1) fila (+ fila 1)) ;filas adyacentes
                            (list (- columna 1) columna (+ columna 1)) ;Columnas adyacentes
                            fila columna filas columnas 0))

(define (contar-minas-cercanas-aux tablero filas-adyacentes columnas-adyacentes fila-orig columna-orig filas columnas contador)
  "Auxiliar recursivo para contar minas cercanas osea adyacentes"
  (if (empty? filas-adyacentes)
      contador ;caso base: todas las filas procesadas
      (contar-minas-cercanas-aux 
       tablero 
       (rest filas-adyacentes) 
       columnas-adyacentes 
       fila-orig columna-orig filas columnas
       (contar-minas-en-fila tablero 
                            (first filas-adyacentes) 
                            columnas-adyacentes 
                            fila-orig columna-orig 
                            filas columnas contador))))

(define (contar-minas-en-fila tablero fila columnas-adyacentes fila-orig columna-orig filas columnas contador)
  "Cuenta minas en una fila de celdas adyacentes"
  (if (empty? columnas-adyacentes)
      contador ; case base: Todas las columnas procesadas
      (if (and (>= fila 0) (< fila filas) ;Fila dentro de límites
               (>= (first columnas-adyacentes) 0)  ;Columna dentro de límites
               (< (first columnas-adyacentes) columnas)
               (not (and (= fila fila-orig) ;No es la celda central
                         (= (first columnas-adyacentes) columna-orig)))
               (es-mina? (obtener-celda tablero fila (first columnas-adyacentes)))) ;Es mina
          (contar-minas-en-fila tablero fila (rest columnas-adyacentes) 
                               fila-orig columna-orig filas columnas 
                               (+ contador 1)) ;Incrementar contador
          (contar-minas-en-fila tablero fila (rest columnas-adyacentes) 
                               fila-orig columna-orig filas columnas 
                               contador)))) ;Mantener contador
;;; <><>><><><><><><>><>>FUNCIÓN PARA CALCULAR MINAS ADYACENTES(End)<><><><<><><><><><><><><><><>

;;; EXPORTAR FUNCIONES
(provide (all-defined-out)) ;Exporta todas las funciones definidas para usarlas en otros archivos.
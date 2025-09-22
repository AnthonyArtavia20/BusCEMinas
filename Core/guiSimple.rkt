#lang racket

(require racket/gui
         "logic.rkt")

;;; VARIABLES GLOBALES
(define tablero-actual #f)
(define frame #f)
(define panel-tablero #f)
(define panel-control #f)
(define botones '())  ;; Lista para almacenar referencia a los botones
(define modo-bandera? #f)  ;; Estado del modo bandera
(define boton-modo-bandera #f)  ;; Referencia al botón de modo bandera
(define total-minas 0)  ;; Total de minas en el tablero
(define banderas-colocadas 0)  ;; Contador de banderas colocadas
(define label-contador-banderas #f)  ;; Etiqueta para mostrar el contador
(define total-celdas 0)  ;; Total de celdas en el tablero
(define celdas-descubiertas 0)  ;; Contador de celdas descubiertas

;;; FUNCIÓN PARA CONTAR MINAS EN EL TABLERO (recursiva)
(define (contar-minas-tablero tablero)
  "Cuenta el total de minas en el tablero de forma recursiva"
  (contar-minas-tablero-aux tablero 0))

(define (contar-minas-tablero-aux tablero acum)
  "Auxiliar recursivo para contar minas en el tablero"
  (if (null? tablero)
      acum
      (contar-minas-tablero-aux (cdr tablero) 
                               (+ acum (contar-minas-fila (car tablero))))))

(define (contar-minas-fila fila)
  "Cuenta minas en una fila de forma recursiva"
  (contar-minas-fila-aux fila 0))

(define (contar-minas-fila-aux fila acum)
  "Auxiliar recursivo para contar minas en una fila"
  (if (null? fila)
      acum
      (contar-minas-fila-aux (cdr fila) 
                           (if (es-mina? (car fila)) 
                               (+ acum 1) 
                               acum))))

;;; FUNCIÓN PARA CONTAR CELDAS DESCUBIERTAS (recursiva)
(define (contar-celdas-descubiertas tablero)
  "Cuenta el total de celdas descubiertas en el tablero de forma recursiva"
  (contar-celdas-descubiertas-aux tablero 0))

(define (contar-celdas-descubiertas-aux tablero acum)
  "Auxiliar recursivo para contar celdas descubiertas en el tablero"
  (if (null? tablero)
      acum
      (contar-celdas-descubiertas-aux (cdr tablero) 
                                    (+ acum (contar-celdas-descubiertas-fila (car tablero))))))

(define (contar-celdas-descubiertas-fila fila)
  "Cuenta celdas descubiertas en una fila de forma recursiva"
  (contar-celdas-descubiertas-fila-aux fila 0))

(define (contar-celdas-descubiertas-fila-aux fila acum)
  "Auxiliar recursivo para contar celdas descubiertas en una fila"
  (if (null? fila)
      acum
      (contar-celdas-descubiertas-fila-aux (cdr fila) 
                                         (if (esta-descubierta? (car fila)) 
                                             (+ acum 1) 
                                             acum))))

;;; FUNCIÓN PARA VERIFICAR VICTORIA
(define (verificar-victoria?)
  "Verifica si el jugador ha ganado el juego"
  (and (= celdas-descubiertas (- total-celdas total-minas))  ; Todas las celdas sin minas descubiertas
       (= banderas-colocadas total-minas)))  ; Todas las minas marcadas con banderas

;;; FUNCIÓN PARA MOSTRAR MENSAJE DE VICTORIA
(define (mostrar-victoria)
  "Muestra mensaje de victoria and deshabilita el tablero"
  (message-box "¡Felicidades!" "¡Has ganado! Todas las minas fueron encontradas." #f '(ok))
  (deshabilitar-tablero))

;;; FUNCIÓN PARA DESHABILITAR TABLERO
(define (deshabilitar-tablero)
  "Deshabilita todos los botones del tablero"
  (deshabilitar-tablero-aux botones))

(define (deshabilitar-tablero-aux lista-botones)
  "Auxiliar recursivo para deshabilitar botones"
  (if (null? lista-botones)
      (void)
      (begin
        (send (third (car lista-botones)) enable #f)
        (deshabilitar-tablero-aux (cdr lista-botones)))))

;;; FUNCIÓN PARA CREAR LA VENTANA PRINCIPAL
(define (crear-ventana-principal)
  (set! frame (new frame% [label "BusCEMinas"] [width 600] [height 480]))
  (set! panel-control (new horizontal-panel% [parent frame] [alignment '(center center)]))
  (set! panel-tablero (new vertical-panel% [parent frame] [alignment '(center center)])))

;;; FUNCIÓN PARA CREAR CONTROLES
(define (crear-controles)
  (crear-boton-modo-bandera)
  (crear-contador-banderas))

;;; FUNCIÓN PARA CREAR BOTÓN DE MODO BANDERA
(define (crear-boton-modo-bandera)
  (set! boton-modo-bandera 
        (new button%
             [parent panel-control]
             [label "Modo: Descubrir"]
             [min-width 120]
             [min-height 30]
             [callback (lambda (b e) (cambiar-modo-bandera))])))

;;; FUNCIÓN PARA CREAR CONTADOR DE BANDERAS
(define (crear-contador-banderas)
  (set! label-contador-banderas
        (new message%
             [parent panel-control]
             [label (string-append "Banderas: " (number->string banderas-colocadas) "/" (number->string total-minas))]
             [min-width 150])))

;;; FUNCIÓN PARA ACTUALIZAR CONTADOR DE BANDERAS
(define (actualizar-contador-banderas)
  (send label-contador-banderas set-label 
        (string-append "Banderas: " (number->string banderas-colocadas) "/" (number->string total-minas))))

;;; FUNCIÓN PARA CAMBIAR MODO BANDERA
(define (cambiar-modo-bandera)
  (set! modo-bandera? (not modo-bandera?))
  (send boton-modo-bandera set-label 
        (if modo-bandera? "Modo: Bandera" "Modo: Descubrir"))
  (printf "Modo bandera: ~a\n" modo-bandera?))

;;; FUNCIÓN AUXILIAR PARA CREAR Y REGISTRAR UN BOTÓN
(define (crear-y-registrar-boton parent fila columna)
  (define boton (new button% 
                    [parent parent] 
                    [label "?"] 
                    [min-width 30] 
                    [min-height 30]
                    [callback (lambda (b e) (procesar-click fila columna))]))
  (set! botones (cons (list fila columna boton) botones))
  boton)

;;; FUNCIÓN PARA CREAR BOTONES (RECURSIVA)
(define (crear-botones-fila parent fila columna-actual total-columnas)
  "Crea los botones de una fila de forma recursiva"
  (if (>= columna-actual total-columnas)
      (void)
      (begin
        (crear-y-registrar-boton parent fila columna-actual)
        (crear-botones-fila parent fila (+ columna-actual 1) total-columnas))))

;;; FUNCIÓN AUXILIAR PARA CREAR UNA FILA
(define (crear-fila parent fila-actual total-columnas)
  (define panel-fila (new horizontal-panel% [parent parent] [alignment '(center center)]))
  (crear-botones-fila panel-fila fila-actual 0 total-columnas))

;;; FUNCIÓN PARA CREAR FILAS DEL TABLERO (RECURSIVA)
(define (crear-filas-tablero parent fila-actual total-filas total-columnas)
  "Crea las filas del tablero de forma recursiva"
  (if (>= fila-actual total-filas)
      (void)
      (begin
        (crear-fila parent fila-actual total-columnas)
        (crear-filas-tablero parent (+ fila-actual 1) total-filas total-columnas))))

;;; FUNCIÓN PARA ACTUALIZAR UN BOTÓN
(define (actualizar-boton fila columna valor)
  "Actualiza el texto de un botón específico"
  (define boton-encontrado (buscar-boton botones fila columna))
  (when boton-encontrado
    (send boton-encontrado set-label valor)))

(define (buscar-boton lista fila-buscada columna-buscada)
  "Busca recursivamente un botón en la lista"
  (if (null? lista)
      #f
      (if (and (= (first (car lista)) fila-buscada) 
               (= (second (car lista)) columna-buscada))
          (third (car lista))
          (buscar-boton (cdr lista) fila-buscada columna-buscada))))

;;; FUNCIÓN PARA PROCESAR CLICK EN MODO BANDERA
(define (procesar-click-bandera fila columna celda)
  "Procesa clicks en modo bandera"
  (if (esta-descubierta? celda)
      (void)
      (cond
        [(tiene-bandera? celda)
         (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                               (poner-bandera celda)))
         (set! banderas-colocadas (- banderas-colocadas 1))
         (actualizar-boton fila columna "?")
         (actualizar-contador-banderas)]
        [(< banderas-colocadas total-minas)
         (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                               (poner-bandera celda)))
         (set! banderas-colocadas (+ banderas-colocadas 1))
         (actualizar-boton fila columna "🚩")
         (actualizar-contador-banderas)
         (if (verificar-victoria?) (mostrar-victoria) (void))]
        [else
         (message-box "Límite alcanzado" 
                      (string-append "No puedes colocar más banderas. Límite: " (number->string total-minas)))])))

;;; FUNCIÓN PARA PROCESAR CLICK EN MODO DESCUBRIR
(define (procesar-click-descubrir fila columna celda)
  "Procesa clicks en modo descubrir"
  (cond
    [(tiene-bandera? celda)
     (message-box "Aviso" "No puedes descubrir una celda con bandera. Cambia a modo descubrir primero.")]
    [(es-mina? celda)
     (actualizar-boton fila columna "X")
     (message-box "Game Over" "¡BOOM! Has perdido." #f '(stop ok))
     (deshabilitar-tablero)]
    [else
     (if (esta-descubierta? celda)
         (void)
         (procesar-celda-no-mina fila columna celda))]))

;;; FUNCIÓN PARA PROCESAR CELDA QUE NO ES MINA
(define (procesar-celda-no-mina fila columna celda)
  "Procesa celdas que no son minas"
  (define minas-adyacentes (obtener-minas-adyacentes celda))
  (actualizar-boton fila columna 
                   (if (= minas-adyacentes 0) 
                       " " 
                       (number->string minas-adyacentes)))
  
  (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                        (descubrir-celda-func celda)))
  (set! celdas-descubiertas (+ celdas-descubiertas 1))
  
  (if (verificar-victoria?) 
      (mostrar-victoria)
      (if (= minas-adyacentes 0)
          (descubrir-adyacentes fila columna)
          (void))))

;;; FUNCIÓN PARA PROCESAR CLICKS
(define (procesar-click fila columna)
  (printf "Click en celda (~a, ~a) - Modo bandera: ~a\n" fila columna modo-bandera?)
  (define celda (obtener-celda tablero-actual fila columna))
  
  (if modo-bandera?
      (procesar-click-bandera fila columna celda)
      (procesar-click-descubrir fila columna celda)))

;;; FUNCIÓN PARA DESCUBRIR CELDAS ADYACENTES (RECURSIVA)
(define (descubrir-adyacentes fila columna)
  "Descubre recursivamente las celdas adyacentes a una celda con 0 minas"
  (descubrir-adyacentes-aux fila columna '(-1 0 1) '(-1 0 1)))

(define (descubrir-adyacentes-aux fila columna lista-df lista-dc)
  "Auxiliar recursivo para descubrir celdas adyacentes"
  (if (null? lista-df)
      (void)
      (begin
        (descubrir-adyacentes-columna fila columna (car lista-df) lista-dc)
        (descubrir-adyacentes-aux fila columna (cdr lista-df) lista-dc))))

(define (descubrir-adyacentes-columna fila columna df lista-dc)
  "Procesa las columnas adyacentes para una fila específica"
  (if (null? lista-dc)
      (void)
      (begin
        (procesar-adyacente (+ fila df) (+ columna (car lista-dc)))
        (descubrir-adyacentes-columna fila columna df (cdr lista-dc)))))

(define (procesar-adyacente f c)
  "Procesa una celda adyacente específica"
  (when (and (>= f 0) (< f (length tablero-actual))
             (>= c 0) (< c (length (car tablero-actual))))
    (define celda (obtener-celda tablero-actual f c))
    (if (or (esta-descubierta? celda) (tiene-bandera? celda))
        (void)
        (procesar-celda-adyacente f c celda))))

;;; FUNCIÓN PARA PROCESAR CELDA ADYACENTE
(define (procesar-celda-adyacente f c celda)
  "Procesa una celda adyacente específica"
  (set! tablero-actual (actualizar-celda tablero-actual f c 
                                        (descubrir-celda-func celda)))
  (set! celdas-descubiertas (+ celdas-descubiertas 1))
  
  (define minas-adyacentes (obtener-minas-adyacentes celda))
  (actualizar-boton f c 
                   (if (= minas-adyacentes 0) 
                       " " 
                       (number->string minas-adyacentes)))
  
  (if (verificar-victoria?) 
      (mostrar-victoria)
      (if (= minas-adyacentes 0)
          (descubrir-adyacentes f c)
          (void))))

;;; FUNCIÓN PRINCIPAL DE LA INTERFAZ
(define (iniciar-interfaz tablero num-filas num-columnas)
  (set! tablero-actual tablero)
  (set! botones '())  ;; Reiniciar lista de botones
  (set! modo-bandera? #f)  ;; Reiniciar modo bandera
  (set! total-minas (contar-minas-tablero tablero))  ;; Contar minas
  (set! banderas-colocadas 0)  ;; Reiniciar contador de banderas
  (set! total-celdas (* num-filas num-columnas))  ;; Calcular total de celdas
  (set! celdas-descubiertas (contar-celdas-descubiertas tablero))  ;; Contar celdas descubiertas iniciales
  (crear-ventana-principal)
  (crear-controles)
  (crear-filas-tablero panel-tablero 0 num-filas num-columnas)
  (send frame show #t))

(define (iniciar-juego tablero)
  (iniciar-interfaz tablero (length tablero) (length (car tablero))))

(provide iniciar-juego)
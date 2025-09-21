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

;;; FUNCIÓN PARA CONTAR MINAS EN EL TABLERO
(define (contar-minas-tablero tablero)
  "Cuenta el total de minas en el tablero"
  (apply + (map (λ (fila) 
                  (length (filter es-mina? fila))) 
                tablero)))

;;; FUNCIÓN PARA CONTAR CELDAS DESCUBIERTAS
(define (contar-celdas-descubiertas tablero)
  "Cuenta el total de celdas descubiertas en el tablero"
  (apply + (map (λ (fila) 
                  (length (filter esta-descubierta? fila))) 
                tablero)))

;;; FUNCIÓN PARA VERIFICAR VICTORIA
(define (verificar-victoria?)
  "Verifica si el jugador ha ganado el juego"
  (and (= celdas-descubiertas (- total-celdas total-minas))  ; Todas las celdas sin minas descubiertas
       (= banderas-colocadas total-minas)))  ; Todas las minas marcadas con banderas

;;; FUNCIÓN PARA MOSTRAR MENSAJE DE VICTORIA
(define (mostrar-victoria)
  "Muestra mensaje de victoria y deshabilita el tablero"
  (message-box "¡Felicidades!" "¡Has ganado! Todas las minas fueron encontradas." #f '(ok))
  (deshabilitar-tablero))

;;; FUNCIÓN PARA DESHABILITAR TABLERO
(define (deshabilitar-tablero)
  "Deshabilita todos los botones del tablero"
  (for-each (λ (boton-info) 
              (send (third boton-info) enable #f)) 
            botones))

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
             [callback (λ (b e) (cambiar-modo-bandera))])))

;;; FUNCIÓN PARA CREAR CONTADOR DE BANDERAS
(define (crear-contador-banderas)
  (set! label-contador-banderas
        (new message%
             [parent panel-control]
             [label (format "Banderas: ~a/~a" banderas-colocadas total-minas)]
             [min-width 150])))

;;; FUNCIÓN PARA ACTUALIZAR CONTADOR DE BANDERAS
(define (actualizar-contador-banderas)
  (send label-contador-banderas set-label 
        (format "Banderas: ~a/~a" banderas-colocadas total-minas)))

;;; FUNCIÓN PARA CAMBIAR MODO BANDERA
(define (cambiar-modo-bandera)
  (set! modo-bandera? (not modo-bandera?))
  (send boton-modo-bandera set-label 
        (if modo-bandera? "Modo: Bandera" "Modo: Descubrir"))
  (printf "Modo bandera: ~a\n" modo-bandera?))

;;; FUNCIÓN PARA CREAR BOTONES (RECURSIVA)
(define (crear-botones-fila parent fila columna-actual total-columnas)
  "Crea los botones de una fila de forma recursiva"
  (if (>= columna-actual total-columnas)
      (void)
      (let ([boton (new button% 
                       [parent parent] 
                       [label "?"] 
                       [min-width 30] 
                       [min-height 30]
                       [callback (λ (b e) (procesar-click fila columna-actual))])])
        ;; Guardar referencia al botón
        (set! botones (cons (list fila columna-actual boton) botones))
        (crear-botones-fila parent fila (+ columna-actual 1) total-columnas))))

(define (crear-filas-tablero parent fila-actual total-filas total-columnas)
  "Crea las filas del tablero de forma recursiva"
  (if (>= fila-actual total-filas)
      (void)
      (let ([panel-fila (new horizontal-panel% [parent parent] [alignment '(center center)])])
        (crear-botones-fila panel-fila fila-actual 0 total-columnas)
        (crear-filas-tablero parent (+ fila-actual 1) total-filas total-columnas))))

;;; FUNCIÓN PARA ACTUALIZAR UN BOTÓN
(define (actualizar-boton fila columna valor)
  "Actualiza el texto de un botón específico"
  (define (buscar-boton lista)
    (cond
      [(empty? lista) #f]
      [(and (= (first (first lista)) fila) 
            (= (second (first lista)) columna))
       (third (first lista))]
      [else (buscar-boton (rest lista))]))
  
  (define boton-encontrado (buscar-boton botones))
  (when boton-encontrado
    (send boton-encontrado set-label valor)))

;;; FUNCIÓN PARA PROCESAR CLICKS
(define (procesar-click fila columna)
  (printf "Click en celda (~a, ~a) - Modo bandera: ~a\n" fila columna modo-bandera?)
  (define celda (obtener-celda tablero-actual fila columna))
  
  (cond
    [modo-bandera?
     ;; Modo bandera: poner/quitar bandera
     (unless (esta-descubierta? celda)
       (cond
         [(tiene-bandera? celda)
          ;; Quitar bandera
          (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                                (poner-bandera celda)))
          (set! banderas-colocadas (- banderas-colocadas 1))
          (actualizar-boton fila columna "?")
          (actualizar-contador-banderas)]
         [(< banderas-colocadas total-minas)
          ;; Poner bandera (solo si no excede el límite)
          (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                                (poner-bandera celda)))
          (set! banderas-colocadas (+ banderas-colocadas 1))
          (actualizar-boton fila columna "🚩")
          (actualizar-contador-banderas)
          (when (verificar-victoria?) (mostrar-victoria))]  ; Verificar victoria al colocar bandera
         [else
          (message-box "Límite alcanzado" 
                       (format "No puedes colocar más banderas. Límite: ~a" total-minas))]))]
    [else
     ;; Modo descubrir: comportamiento normal
     (cond
       [(tiene-bandera? celda)
        (message-box "Aviso" "No puedes descubrir una celda con bandera. Cambia a modo descubrir primero.")]
       [(es-mina? celda)
        (actualizar-boton fila columna "X")
        (message-box "Game Over" "¡BOOM! Has perdido." #f '(stop ok))
        (deshabilitar-tablero)]
       [else
        (unless (esta-descubierta? celda)  ; Solo procesar si no está descubierta
          (define minas-adyacentes (obtener-minas-adyacentes celda))
          (actualizar-boton fila columna 
                           (if (= minas-adyacentes 0) 
                               " " 
                               (number->string minas-adyacentes)))
          
          ;; Marcar celda como descubierta en el tablero lógico
          (set! tablero-actual (actualizar-celda tablero-actual fila columna 
                                                (descubrir-celda-func celda)))
          (set! celdas-descubiertas (+ celdas-descubiertas 1))
          
          ;; Verificar victoria después de descubrir celda
          (when (verificar-victoria?) (mostrar-victoria))
          
          ;; Si es 0, descubrir celdas adyacentes automáticamente
          (when (= minas-adyacentes 0)
            (descubrir-adyacentes fila columna)))])]))

;;; FUNCIÓN PARA DESCUBRIR CELDAS ADYACENTES (RECURSIVA)
(define (descubrir-adyacentes fila columna)
  "Descubre recursivamente las celdas adyacentes a una celda con 0 minas"
  (define (procesar-adyacente f c)
    (when (and (>= f 0) (< f (length tablero-actual))
               (>= c 0) (< c (length (first tablero-actual))))
      (define celda (obtener-celda tablero-actual f c))
      (unless (or (esta-descubierta? celda) (tiene-bandera? celda))
        ;; Marcar como descubierta
        (set! tablero-actual (actualizar-celda tablero-actual f c 
                                              (descubrir-celda-func celda)))
        (set! celdas-descubiertas (+ celdas-descubiertas 1))
        
        (define minas-adyacentes (obtener-minas-adyacentes celda))
        (actualizar-boton f c 
                         (if (= minas-adyacentes 0) 
                             " " 
                             (number->string minas-adyacentes)))
        
        ;; Verificar victoria después de descubrir celda
        (when (verificar-victoria?) (mostrar-victoria))
        
        ;; Si también es 0, seguir expandiendo
        (when (= minas-adyacentes 0)
          (descubrir-adyacentes f c)))))
  
  ;; Procesar las 8 celdas adyacentes
  (for ([df (in-list '(-1 0 1))])
    (for ([dc (in-list '(-1 0 1))])
      (unless (and (= df 0) (= dc 0))
        (procesar-adyacente (+ fila df) (+ columna dc))))))

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
  (iniciar-interfaz tablero (length tablero) (length (first tablero))))

(provide iniciar-juego)
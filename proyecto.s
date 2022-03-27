PROCESSOR 16F887

; PIC16F887 Configuration Bit Settings

; Assembly source line config statements

; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit
  CONFIG  CP = OFF              ; Code Protection bit
  CONFIG  CPD = OFF             ; Data Code Protection bit
  CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits
  CONFIG  IESO = OFF            ; Internal External Switchover bit
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit
  CONFIG  LVP = OFF              ; Low Voltage Programming Enable bit

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits

// config statements should precede project file includes.
#include <xc.inc>
;-------------- macros-------------------
restar_tmr1 macro
    movlw   0xB
    movwf   TMR1H
    movlw   0x47
    movwf   TMR1L
    bcf	    TMR1IF
    endm
restar_tmr0 macro
    banksel PORTA
    movlw 61     ; esta a 50ms
    movwf TMR0
    bcf   T0IF
    endm
    
preparar_displays macro displayss, displayss2, displayss3, displayss4
    movf displayss, w
    call tabla
    movwf display_var
    
    movf displayss2, w
    call tabla
    movwf display_var+1
    
    movf displayss3, w
    call tabla
    movwf display_var+2
    
    movf displayss4, w
    call tabla
    movwf display_var+3
    endm
;---------------------- variables -------------------------
;--------------------- estados -------------------
  PSECT udata_bank0
   edit:        DS 1 ;para editar el estado
   estado:      DS 2 ;para identificar el estado
   incrementar: DS 1
   decrementar: DS 1
;--------------------- displays --------------
   var:         DS 1 ;display
   nibble:      DS 2 ;display
   banderas:    DS 1 ;display
   display_var:	DS 4 ;display
   
;---------------------- tiempos --------------
   segs:        DS 1 ;tiempo
   inr:	        DS 1 ;tiempo
   segundo:	DS 1 ;tiempo
   minutos:	DS 1 ;tiempo
   horas:	DS 1 ;tiempo
   fake_minutos:DS 1 ;tiempo
   cont:        DS 2 ;tiempo
   inicio:      DS 1 ;tiempo
   dechoras:	DS 1 ;tiempo
;--------------------- ver inter--------------
   PSECT udata_shr ;common memory
   W_TEMP:	DS 1 ;1 byte
   STATUS_TEMP: DS 1
; -------------- VECTOR RESET ---------------
PSECT resVect, class=CODE, abs, delta=2
ORG 00h		; posición 0000h - Vector de reset
resVect:
    PAGESEL main
    GOTO main
; -------------- INTERUPCCIONES RESET ---------------
PSECT intVect, class=CODE, abs, delta=2
ORG 04h		; posición 0004h para las interrupciones
push:
   movwf  W_TEMP
   swapf  STATUS, W
   movwf  STATUS_TEMP

    
    
isr:

   
   btfsc  T0IF
   call   int_T0
   
   btfsc  TMR1IF
   call   TMR1_inc
   
   btfsc  TMR2IF
   call   TMR2_inc
   btfsc  RBIF
   call   menu


    
pop:
   swapf  STATUS_TEMP, W
   movwf  STATUS
   swapf  W_TEMP, F
   swapf  W_TEMP, W
   retfie

;-----------------subrutinas de interrrupcion-------------

int_T0:
    restar_tmr0
    clrf  PORTD
    call  mostrar_displays
    return


;-----------------timer1 interrrupcion-------------
TMR1_inc:
    restar_tmr1
    incf    segs ;incremeta 2 veces por segundo
    call segundos
    return
segundos:
    btfss   segs, 1
    return
    clrf    segs
    incf    segundo
    call  segundos_a_minutos
    return
segundos_a_minutos:
    btfss   segundo, 1
    return
    btfss   segundo, 3
    return
    clrf    segundo
    incf    inr, 1
    call    minuto
    return
minuto:
    btfss   inr, 1
    return
    btfss   inr, 2
    return
    clrf    inr
    incf    fake_minutos
    incf    minutos
    call    minutos_a_horas
    movf    minutos, w
    return
minutos_a_horas:
    btfss   fake_minutos, 1
    return
    btfss   fake_minutos, 3
    return
    clrf    fake_minutos
    clrf    minutos
    incf    cont
    call    hora
    return
hora:
    btfss   cont, 1
    return
    btfss   cont, 2
    return
    clrf    cont
    incf    horas
    call    hora_a_decena
    return
hora_a_decena:
    btfss   cont, 1
    return
    btfss   cont, 3
    return
    clrf    cont
    incf    dechoras
    return
;-----------------menu-----------------------------
menu:
    btfss estado, 0
    goto  estado_0_inter  ;editar hora
    goto  estado_1_inter  ;timer (en proceso)
return
estado_0_inter:
    btfss   PORTB,1
    incf    edit
    btfss   PORTB,0
    bsf	    estado, 0
    bcf	    RBIF
    return
estado_1_inter:
    btfss   PORTB,1
    decf    PORTA
    btfss   PORTB,0
    bcf	    estado, 0
    bcf	    RBIF
    return

;-----------------timer2 interrrupcion-------------
TMR2_inc:
    bcf	    TMR2IF
    return
//---------------------------INDICE DISPLAY 7SEG--------------------------------
PSECT tabla, class = CODE, abs, delta = 2
ORG 150h 

tabla:
    CLRF PCLATH
    BSF PCLATH, 0           ; PCLATH en 01
    ANDLW 0X0F
    ADDWF PCL               ; PC = PCLATH + PCL | Sumamos W al PCL para seleccionar un dato en la tabla
    retlw 00111111B         ; 0
    retlw 00000110B         ; 1
    retlw 01011011B         ; 2
    retlw 01001111B         ; 3
    retlw 01100110B         ; 4
    retlw 01101101B         ; 5
    retlw 01111101B         ; 6
    retlw 00000111B         ; 7
    retlw 01111111B         ; 8 
    retlw 01101111B         ; 9
    retlw 01110111B         ; A
    retlw 01111100B         ; b
    retlw 00111001B         ; C
    retlw 01011110B         ; D
    retlw 01111001B         ; C
    retlw 01110001B         ; F
;-----------------configuracion--------------------------
main:

  call	    config_io
  call	    config_timer0
  call	    config_TMR1
  call      config_TMR2
  call	    config_reloj 
  call	    config_inter_tmrs
  call	    config_iocb
  banksel   PORTA

;-------------------loop Principal-------------------------
loop:
    preparar_displays cont, minutos, horas, dechoras
    btfss estado, 0
    goto  estado_0
    goto  estado_1
estado_0:
    bsf PORTE, 0
    bcf PORTE, 1
    call editar_hora
goto loop
estado_1:
    bsf PORTE, 1
    bcf PORTE, 0
   
goto loop
   
   goto    loop	;loop forever

;------------------sub rutinas---------------------------- 
    
config_io:	    ;configuracion general de entradas y salidas
   banksel  ANSEL
   clrf	    ANSEL     
   clrf	    ANSELH

   banksel  TRISA
   clrf	    TRISA
   clrf	    TRISC
   
   bcf      TRISE, 0
   bcf      TRISE, 1
   
   bcf      TRISD, 0
   bcf      TRISD, 1
   bcf      TRISD, 2
   bcf      TRISD, 3
   
   bsf      TRISB, 0
   bsf      TRISB, 1
   bsf      TRISB, 2
   bsf      TRISB, 3
   
   bcf	    OPTION_REG, 7
   bsf	    WPUB, 0
   bsf	    WPUB, 1
   bsf	    WPUB, 2
   bsf	    WPUB, 3

 
   banksel  PORTA
   clrf	    PORTA
   clrf	    PORTB
   clrf	    PORTC
   clrf	    PORTD
   clrf	    PORTE
   return
   
   
config_iocb:
    banksel  IOCB
    bsf	     IOCB, 0
    bsf	     IOCB, 1
    bsf	     IOCB, 2
    bsf	     IOCB, 3
    
    banksel PORTA
    movf    PORTB, w
    bcf	    RBIF
    return
;------------------------------reloj config-------------------------    

config_reloj:  ; configuracion de reloj interno
    banksel OSCCON  ;cambio de banco al OSCON
    bsf	    IRCF2   ;OSCCON, 6
    bsf	    IRCF1   ;OSCCON, 5
    bcf	    IRCF0   ;OSCCON, 4
    bsf	    SCS	    ;el OSCCON esta actualmente en 4Hz
    return
;------------------------------timer0 config-------------------------    
config_timer0:
    banksel TRISA
    bcf     T0CS
    bcf     PSA
    bsf     PS2
    bsf     PS1
    bsf     PS0
    restar_tmr0
    return
;------------------------------timer1 config-------------------------
config_TMR1:
    banksel T1CON  
    bcf     TMR1GE  
    bsf     T1CKPS1  
    bsf     T1CKPS0  
    bcf     T1OSCEN  
    bcf     TMR1CS  
    bsf     TMR1ON  
    restar_tmr1
    return
;------------------------------timer2 config-------------------------
config_TMR2:  
    banksel PORTA	    ; Cambiamos a banco 00
    bsf	    TOUTPS3	    ; postscaler 1:16
    bsf	    TOUTPS2
    bsf	    TOUTPS1
    bsf	    TOUTPS0
    
    bsf	    TMR2ON	    ; prendemos TMR2
    
    bsf	    T2CKPS1	    ; prescaler 1:16
    bsf	    T2CKPS0
    
    banksel PR2		    ; Cambiamos a banco 01
    movlw   195		    ; Valor para interrupciones cada 50ms
    movwf   PR2		    ; Cargamos litaral a PR2
    clrf    TMR2
    bcf	    TMR2IF
    return


;------------------------------inter, timers config-------------------------    
config_inter_tmrs:
    banksel TRISA  
    bsf     TMR1IE 
    bsf     TMR2IE
    banksel PORTA
    bsf	    T0IE
    bcf     T0IF
    bcf     TMR1IF	
    bcf     TMR2IF	
    bsf     PEIE  
    bsf     GIE   
    return
;--------------------------- estados-------------------
editar_hora:
    btfss estado+1, 0
    goto  estado_0_h
    goto  estado_1_h
estado_0_h: ;incrementar
    btfss   PORTB,1
    incf    PORTA
    btfss   PORTB,2
    bsf	    estado+1, 0

    return
estado_1_h: ;incrementar
    btfss   PORTB,1
    incf    PORTA
    btfss   PORTB,2
    bcf	    estado+1, 0
    
    return
;--------------------------- displays------------------
mostrar_displays:
    bcf	    PORTD, 0
    bcf	    PORTD, 1
    bcf	    PORTD, 2
    bcf	    PORTD, 3
    
    btfsc   banderas, 1
    goto    display_1
    
    btfsc   banderas, 2
    goto    display_2
    
    btfsc   banderas, 3
    goto    display_3
    goto    display_0
    
display_0:			
    MOVF    display_var, W	
    MOVWF   PORTC		
    BSF	    PORTD, 3	        
    CLRF    banderas			
    BSF	    banderas, 1
    return

display_1:
    MOVF    display_var+1, W	
    MOVWF   PORTC	
    BSF	    PORTD, 2		
    CLRF    banderas			
    BSF	    banderas, 2
    return
    
display_2:
    MOVF    display_var+2, W	
    MOVWF   PORTC		
    BSF	    PORTD, 1		
    CLRF    banderas			 
    BSF	    banderas, 3
    return
    

display_3:
    MOVF    display_var+3, W	
    MOVWF   PORTC		
    BSF	    PORTD, 0		
    CLRF    banderas			
    BSF	    banderas, 0
    return
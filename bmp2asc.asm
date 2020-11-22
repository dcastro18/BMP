
; Tarea Programada - BMP to Ascii

; David Castro H - 2018105813

;------------------------------------------------------------------------------

include macroBMP.cbc
extrn  print:Far,modeWrite:Far,exit:Far

SPila Segment para Stack 'Stack'
     db 64 Dup ('SegStack ')
SPila EndS

;------------------------------------------------------------------------------


SDato Segment para public 'Data'


	;----- VARIABLES DEL BMP

		Header				label 	word
		HeadBuff    		db 		54 dup('H')
		palBuff     		db 		1024 dup('P')
		ScrLine     		db 		640 dup(0)
		BMPStart    		db 		'BM'

		PalSize     		dw 		?
		BMPHeight   		dw 		?
		BMPWidth    		dw 		?

	;------PARAMETROS QUE RECIBEN
		bmp         		 db    	0FFh Dup (?)
		archivo     		 db    	'archivo.txt'
		instruction 		 db    	'?'
		password    		 db    	0FFh Dup (?)
		text        		 db    	0FFh Dup (?)
		textSize			 db	  	00
		LineCommand          db    	0FFh Dup (?)

	;------MENSAJES PARA GUIA DE USUARIO /?
		info0  				db    	10,13,'-------------------->GUIA DE USUARIO<---------------------------'
		info1  				db    	10,13,'Bienvenido a la guia de usuario:',10,13
		info2  				db    	10,13,'-> Para convertir la imagen a Ascii el formato es el siguiente:'   
		info3  				db    	10,13,'        /a /bmp:archivo.bmp',10,13
		info4  				db    	10,13,'-> Para girar la imagen el formato es el siguiente:'
		info5  				db    	10,13,'        /d /bmp:archivo.bmp',10,13   
		info6  				db    	10,13,'        -bmp = el nombre del archivo .bmp que se utilizara.'      
		info7 				db    	10,13,'----------------------------------------------------------------',10,13,'$'
		error0 				db    	10,13,'Comando incorrecto, digite /? o /h o /H para mas ayuda.$'
	
		msgInvBMP   		db 		"Archivo BMP invalido.$"
		openFileError		db 		"Error al abrir archivo.$"
		error1   		    db		'No se pudo crear$'
		error2     	        db    	"No se pudo escribir$"
		error3   	        db    	"No se pudo cerrar$"
		error4     	        db  	'No se pudo abrir$'
		filehandle 		    dw 		?
		txthandle		    dw		?

SDato EndS

;-------------------------------------------------------------------------------------------------------------------------------------------------------

SCodigo Segment para public 'Code'                          
		Assume CS:SCodigo, SS:SPila, DS:SDato


;------ MUESTRA LA GUIA DE USUARIO

showInfo Proc far                    
		lea	dx,info0                  ;Mueve al dx el desplazamiento del msj que quiero mostrar en este caso la guia de usuario
		jmp	print                     ;y salta a la etiqueta print para que imprima el msj
showInfo EndP


;------ LEE LA LINEA DE PARAMETROS

GetCommanderLine Proc Near
		LongLC	EQU		80h                           ;apunta a la direccion del tamano de la linea de comandos(en 80h estara el tamano)
		ListPush <Es, Di, Si, Cx, Bp>      
		Mov	Bp,Sp 
		Mov	Di,12[Bp]                                 ;Cantidad de caracteres que va a llevar esa palabra (carga el desplazamiento)
		Mov	Ax,14[Bp]                                 ;La linea de arriba y abajo es para apuntar a la variable Es:Di (carga el segmento)
		Mov	Es,Ax      
		Xor	Cx,Cx                                     ;Limpio el cx para que no haya basura
		Mov	Cl,Byte Ptr Ds:[LongLC]                   ;Uso el cl porque el maximo es de 255 entonces asi encaja
		Mov	Si,3[LongLC]                              ;apunta al primer caracter, dos = uno por la posición 81h y uno más por el espacio en blanco, ahora lo cambie a 3 para no evaluar el /
		Rep	Movsb                                     ;Agarra cada caracter , y lo copia en Di:Es para que quede en LineCommand
		ListPop <Bp, Bx, Si, Di, Es>
		Ret   
GetCommanderLine EndP




;------ EVALUA CADA PARAMETRO

EvalLineCommand Proc Far
		mov	cl,[si]       			;mueve al cl el caracter a evaluar

		cmp	cl,'a'
		jz 	saveInstruction
		cmp	cl,'r'
		jz		saveInstruction
		cmp	cl,'i'
		jz		saveInstruction
		cmp	cl,'d'
		jz		saveInstruction
		cmp	cl,'?'
		jz 		showInfo
		cmp	cl,'h'
		jz		showInfo
		cmp	cl,'H'
		jz  	showInfo

		lea	dx,error0
		jmp	print
		saveInstruction:
			lea	di, instruction
			mov	[di],cl
			inc	si
			jmp	readMore
		readMore:
			mov	cl,[si]      		;mueve al cl el caracter a evaluar
			cmp cl,' '
			jz  ignore
			cmp cl,'/'
			jz  getParam
		getParam:
			xor dx,dx
			inc si
			mov cl,[si] 

			cmp cl,'b'
			jz  getBMP

		getBMP:
			lea di,bmp
			add si,4
			jmp saveParam
		saveParam:
			mov cl,[si]
			cmp cl,10
			jz  done
			cmp cl,13
			jz  done
			cmp cl,'/'
			jz  getParam
			mov [di],cl
			inc si
			inc di
			cmp dx,1h
			jne	saveParam
			inc textSize
			jmp saveParam
		ignore:
			inc si
			jmp readMore
		done:
			ret
EvalLineCommand EndP

;------ABRE EL ARCHIVO 

openFile proc                      
		mov	ah,3dh
		mov	al,02h                   ;establece el modo de abrir 
		lea	dx,bmp                   ;señala la direccion donde esta el nombre
		int	21h
		mov	dx, offset error4
		jc print
		mov	bx,ax
		mov	filehandle,ax
		ret
openFile endp


;================ LEE EL ENCABEZADO DEL BMP ================

ReadHeader proc	

		mov	  ah,3fh
		mov	  cx,54
		mov	  dx,offset Header
		int	  21h

		jc	  RHdone			        ; Si no se leyeron los 54 bytes del encabezado, terminar.

		mov	  ax,header[0Ah]        	; AX = Desplazamiento de la direccion donde comienza la imagen

		sub	  ax,54                     ; Restar la longitud del encabezado de la imagen

		; Dividir el resultado entre 4
		shr   ax,1
		shr   ax,1
		mov	  PalSize,ax					; Obtener el numero de colores del BMP
		
		; Guardar el ancho del BMP en BMPWidth
		mov	  ax,header[12h]
		mov	  BMPWidth,ax  
		
		; Guardar la altura del BMP en BMPHeight
		mov	  ax,header[16h]    
		mov	  BMPHeight,ax 

		RHdone:
			ret
			
ReadHeader endp


; ================ LEER LA PALETA DE VIDEO ================

; Este procedimiento recorre el buffer de la paleta y 
; envia informacion de la paleta a los registros de 
; video. Se envia un byte a traves del puerto 3C8h que
; contiene el primer indice a modificar de la paleta  
; de colores. Despues, se envia informacion acerca de 
; los colores (RGB) a traves del puerto 3C9h

ReadPal proc
		mov	ah,3fh
		mov	cx,PalSize         ; Numero de colores de la paleta en CX
		
		; Multiplicar este numero por 4 para obtener tamanio de la paleta en bytes
		shl	cx,1
		shl	cx,1
		
		mov	dx,offset palBuff  ; Poner la paleta en el buffer
		int	21h

		; SI apunta al buffer que contiene a la paleta.
		mov	si,offset palBuff
		; Numero de colores a enviar en CX
		mov	cx,PalSize
		mov	dx,3c8h
		; Comenzar en el color 0
		mov	al,0
		out	dx,al
		; DX = 3C9h
		inc	dx
		sndLoop:
			; Nota: los colores en un archivo BMP se guardan como
			; BGR y no como RGB

			; Obtener el valor para el rojo
			mov	 al,[si+2]
			; El maximo es 255, pero el modo de video solamente
			; permite valores hasta 63, por lo tanto dividimos 
			; entre 4 para obtener un valor valido
			shr	 al,1
			shr	 al,1
			; Mandar el valor del rojo por el puerto 3C9h
			out	 dx,al
			; Obtener el valor para el verde
			mov	 al,[si+1]
			shr	 al,1
			shr	 al,1
			; Mandar el valor del verde por el puerto
			out	 dx,al
			; Obtener el valor para el azul
			mov	 al,[si]
			shr	 al,1
			shr	 al,1
			; Enviarlo por el puerto
			out	 dx,al

			; Apuntar al siguiente color
			; (Hay un caracter nulo al final de cada color)
			add	 si,4
			loop sndLoop
		ret
ReadPal endp

;-- COLOCA EL PUNTERO PARA PINTAR

movePointer proc
		mov	ah,42h
		mov	al,00
		mov	dx,76h
		int	21h
		mov	filehandle,bx
		mov	dx, offset error2   
		jc 	print
		ret
movePointer endp


inicio:
		Mov		Ax,Seg LineCommand   
		Push	Ax
		Lea		Ax,LineCommand
		Push  	Ax

		call	GetCommanderLine
		push  	es                      ;Guarda el psps
		push  	es
		mov   	ax,SDato                ;Inicializa el seg de datos        
		push  	ax
		push  	ax
		pop   	ds;
		pop   	es;
		
		lea 	si,LineCommand
		call 	EvalLineCommand
		mov		ah,00
		mov 	al,12h
		int 	10h

		
		call 	OpenFile
		; ;call createFile
		call 	ReadHeader
		call 	ReadPal
		;call writeFile
		cmp 	instruction,'a'
		je		a
		cmp 	instruction,'r' ; por ahora pruebo con r aunque la idea es que lo despliegue normal
		je		r
		
		a:
			;call desesteg
			jmp	 finish
		r:
			;call inverso
			jmp  finish
		d:
			;call giroDer
			jmp  finish
		l:
			;call giroIzq
			jmp  finish
		
		finish:
			call	movePointer
			;call	showBMP
			;mov	bx,filehandle
			;call	closeFile

			; Wait for key press
			mov	ah,1
			int	21h    
		  
			;call   modeWrite
		 	jmp	   exit

wrongCommand:                        ;Si el usuario digita un parametro erroneo entonces no entra a ningun cmp y cae aqui
		mov	dx,offset error0          ;Mueve al dx el desplazamiento del msj que quiero imprimir
		jmp	print                          ;y salta a la etiqueta print para que lo imprima


End inicio 
SCodigo EndS
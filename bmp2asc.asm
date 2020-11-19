
; Tarea Programada - BMP to Ascii

; David Castro H - 2018105813

;------------------------------------------------------------------------------


include macroBMP.cbc

SPila Segment para Stack 'Stack'
     db 64 Dup ('SegStack ')
SPila EndS

;------------------------------------------------------------------------------


SDato Segment para public 'Data'

	
	
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
		info3  				db    	10,13,'        /a /bmp:archivo.bmp /archivo:archivo.text',10,13
		info4  				db    	10,13,'-> Para girar la imagen el formato es el siguiente:'
		info5  				db    	10,13,'        /d /bmp:archivo.bmp',10,13
		info6  				db    	10,13,'    -mensaje = el mensaje a encriptar.'    
		info7  				db    	10,13,'    -archivo = el nombre del archivo .txt que se utilizara.'
		info8  				db    	10,13,'    -bmp     = el nombre del archivo .bmp que se utilizara.'      
		info9  				db    	10,13,'----------------------------------------------------------------',10,13,'$'
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


showInfo Proc far                     ;Esta etiqueta mueve al muestra la guia de usuario.
		lea	dx,info0                  ;Mueve al dx el desplazamiento del msj que quiero mostrar en este caso la guia de usuario
		;jmp	print                     ;y salta a la etiqueta print para que imprima el msj
		print
showInfo EndP

prueba Proc far
	print
prueba EndP


;------ABRE EL ARCHIVO 

openFile proc                      
		mov	ah,3dh
		mov	al,02h                     ;establece el modo de abrir 
		lea	dx,bmp                   ;señala la direccion donde esta el nombre
		int	21h
		mov	dx, offset error4
		jc prueba
		mov	bx,ax
		mov	filehandle,ax
		ret
openFile endp

inicio:
	mov		Ax,Seg LineCommand   
	push	Ax
	lea		Ax,LineCommand	
	push  	Ax

	exit


End inicio 
SCodigo EndS
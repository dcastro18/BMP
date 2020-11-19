
; Tarea Programada - BMP to Ascii

; David Castro H - 2018105813

;------------------------------------------------------------------------------


include macrosBMP.cbc


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
		info2  				db    	10,13,'-> Para esteganografiar una imagen el formato es el siguiente:'   
		info3  				db    	10,13,'        /e /t:mensaje /c:clave /bmp:archivo.bmp',10,13
		info4  				db    	10,13,'-> Para desencriptar un mensaje el formato es el siguiente:'
		info5  				db    	10,13,'        /d /c:clave /bmp:archivo.bmp',10,13
		info6  				db    	10,13,'    -mensaje = el mensaje a encriptar.'    
		info7  				db    	10,13,'    -clave   = la clave para encriptar o desencriptar.(0-15)'
		info8  				db    	10,13,'    -bmp     = el nombre del archivo .bmp que se utilizara.'      
		info9  				db    	10,13,'----------------------------------------------------------------',10,13,'$'
		error0 				db    	10,13,'Comando incorrecto, digite /? o /h o /H para mas ayuda.$'

SDato EndS

;-------------------------------------------------------------------------------------------------------------------------------------------------------

SCodigo Segment para public 'Code'                          
		Assume CS:SCodigo, SS:SPila, DS:SDato

;------ABRE EL ARCHIVO 

openFile proc                      
		mov	ah,3dh
		mov	al,02h                     ;establece el modo de abrir 
		lea	dx,bmp                   ;señala la direccion donde esta el nombre
		int	21h
		mov	dx, offset error4
		jc print
		mov	bx,ax
		mov	filehandle,ax
		ret
openFile endp


End inicio 
SCodigo EndS
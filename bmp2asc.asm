
; Tarea Programada - BMP to Ascii

; Esteban Madrigal M - 2018154123
; David Castro H - 2018105813

;------------------------------------------------------------------------------

SPila Segment para Stack 'Stack'
     db 64 Dup ('SegStack ')
SPila EndS

;------------------------------------------------------------------------------


SDato Segment para public 'Data'
	
	;------PARAMETROS QUE RECIBEN
		bmp         		     db    	0FFh Dup (?)
		archivo     		 db    	'archivo.txt0'
		instruction 		 db    	'?'
		password    		 db    	0FFh Dup (?)
		text        		     db    	0FFh Dup (?)
		textSize			 db	  	00
		LineCommand    db    	0FFh Dup (?)

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
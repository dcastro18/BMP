
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
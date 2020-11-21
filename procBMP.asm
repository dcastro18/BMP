;Programa Principal: bmp2asc.asm

;--------------- Libreria de Procedimientos--------------- 

Procedimientos Segment

    public print,exit,modeWrite
    Assume cs:Procedimientos

;---------------------------------------------------------

print Proc Far 
	mov ah,0
    mov al,2
    int 10h
    mov ah,09h     
    int 21h
    jmp exit                              
print EndP 

modeWrite Proc Far
	mov ah,0
    mov al,2
    int 10h
	retf
modeWrite EndP

exit Proc Far
    mov ax, 4c00h
    int 21h
exit EndP

;---------------------------------------------------------

Procedimientos EndS
end
	org $fd00
digitTable:
digit0:
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit1:
	.byte %00000100;| X  
	.byte %00001100;|XX  
	.byte %00001100;|XX  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit2:
	.byte %00000100;| X  
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00000010;|  X 
	.byte %00000110;| XX 
	.byte %00000110;| XX 
	.byte %00001100;|XX  
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit3:
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00000010;|  X 
	.byte %00000110;| XX 
	.byte %00000100;| X  
	.byte %00000010;|  X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit4:
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit5:
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00001100;|XX  
	.byte %00001100;|XX  
	.byte %00000110;| XX 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000100;| X  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit6:
	.byte %00000110;| XX 
	.byte %00001110;|XXX 
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00001100;|XX  
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit7:
	.byte %00001110;|XXX 
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00000110;| XX 
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00001000;|X   
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit8:
	.byte %00000100;| X  
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00000100;| X  
	.byte %00000100;| X  
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000100;| X  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digit9:
	.byte %00000100;| X  
	.byte %00001100;|XX  
	.byte %00001110;|XXX 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001010;|X X 
	.byte %00001110;|XXX 
	.byte %00000110;| XX 
	.byte %00000010;|  X 
	.byte %00000010;|  X 
	.byte %00001110;|XXX 
	.byte %00001100;|XX  
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
digitBlank:
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    
	.byte %00000000;|    

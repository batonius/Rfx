/*Rfx include file*/
#ifndef RFX_H
#define RFX_H
#define XOR(x,y) ((x) ? !(y) : (y))
#define BOOL char
#define TRUE 1
#define FALSE 0

extern void __rfx_next();
#ifdef AVR
char getPortByte(int port)
{
    switch(port)
    {
    case 2:
        return PINB;
    case 4:
        return PIND;
    }
}
  
void setPortByte(int port, char byte)
{
    switch(port)
    {
    case 2:
        PORTB = byte;
        break;
    case 4:
        PORTD = byte;
        break;
    }
}

char getPortBit(int port, int b)
{
    return (getPortByte(port) & (1<<b));
}

void setPortBit(int port, int b, char val)
{
    switch(port)
    {
    case 2:
        if(val)
            PORTB |= 1<<b;
        else
            PORTB &= ~(1<<b);
        break;
    case 4:
        if(val)
            PORTD |= 1<<b;
        else
            PORTD &= ~(1<<b);
        break;
    }
}
#endif
#endif

/******************************************************************************
 *
 * Copyright:
 *    (C) 2000 - 2008 Embedded Artists AB
 *
 *****************************************************************************/


/******************************************************************************
 * Includes
 *****************************************************************************/


//#include <general.h>
//#include "lpc24xx.h"
#include "lcd_hw.h"
#include "lpc2468_registers.h"

//#include "delay.h"
//#include <config.h>
//#include <printf_P.h>

/******************************************************************************
 * Typedefs and defines
 *****************************************************************************/

#define ACTIVATE_CS   IOCLR0 = 0x10000
#define DEACTIVATE_CS IOSET0 = 0x10000
#define SET_RS        IOSET0 = 0x80000
#define RESET_RS      IOCLR0 = 0x80000

/******************************************************************************
 * Implementation of local functions
 *****************************************************************************/
void mdelay(tU32 ms)
{
  /*
   * setup timer #1 for delay
   */
  T1TCR = 0x02;          //stop and reset timer
  T1PR  = 0x00;          //set prescaler to zero
  T1MR0 = ms * (72000000 / 1000);
  T1IR  = 0xff;          //reset all interrrupt flags
  T1MCR = 0x04;          //stop timer on match
  T1TCR = 0x01;          //start timer

  //wait until delay time has elapsed
  while(T1TCR & 0x01)
    ;
}


/*****************************************************************************
 *
 * Description:
 *    Send data on the SPI interface.
 *
 * Params:
 *    [in] data - data to send
 *
 ****************************************************************************/
void spiSend(tU8 data)
{
  unsigned int failsafe;

  S0SPDR = data;
  failsafe = 0;
  while(((S0SPSR & 0x80) == 0) && (failsafe < 5000))
    failsafe++;

  if(failsafe >= 5000)
  {
    S0SPCCR = 8;
    S0SPCR  = 0x38;
  }
}

/*****************************************************************************
 *
 * Description:
 *    Write to display register
 *
 * Params:
 *    [in] addr - register address
 *    [in] data - data to write
 *
 ****************************************************************************/
void writeToReg(tU16 addr, tU16 data)
{
  RESET_RS;
  ACTIVATE_CS;
  spiSend(0);
  spiSend(addr & 0xff);
  DEACTIVATE_CS;  

  SET_RS;
  ACTIVATE_CS;
  spiSend(data >> 8);  
  spiSend(data & 0xff);
  DEACTIVATE_CS;  

  RESET_RS;
  ACTIVATE_CS;
  spiSend(0);
  spiSend(0x22);
  DEACTIVATE_CS;  
}


/*****************************************************************************
 *
 * Description:
 *    Initialize the hardware for the LCD controller.
 *
 ****************************************************************************/
void lcd_hw_init(void)
{

  //Turn on power to LCD
  FIO2DIR |= 0x1;
  FIO2SET  = 0x1;

  //set P1.18 as PWM output (PWM1.1, second alternative function)
  PINSEL3 &= 0xFFFFFFCF;
  PINSEL3 |= 0x00000020;
  PWM1PR  = 0x00;     //no prescaling
  PWM1MCR = 0x02;     //reset counter if MR0 match
  PWM1MR0 = 0x3000;   //period time equal about 5 ms
  PWM1MR1 = 0x0000;
  PWM1LER = 0x03;     //latch MR0 and MR1
  PWM1PCR = 0x0200;   //enable PWMENA1
  PWM1TCR = 0x09;     //enable counter and PWM

  //init SPI #0
  PINSEL0 |= 0xc0000000;
  PINSEL1 |= 0x0000003c;

  IODIR0  |= 0x90000;
  IOSET0   = 0x90000;

  S0SPCCR = 8;    
  S0SPCR  = 0x38;
  
  PINSEL10 = 0;
  PINSEL0  = (PINSEL0 & 0xFFF000FF) | 0x00055500;
  PINSEL3  = (PINSEL3 & 0xF00000FF) | 0x05555500; 
  PINSEL4  = (PINSEL4 & 0xF0300000) | 0x054FFFFF;
  PINSEL9  = (PINSEL9 & 0xF0FFFFFF) | 0x0A000000;
  
  PINSEL11 = 0x0000000d;

}

/*****************************************************************************
 *
 * Description:
 *    Initialize the LCD display
 *
 ****************************************************************************/
void lcd_display_init(void)
{  
  
  writeToReg (0x00,0x0001);
  mdelay(20);
  writeToReg (0x03,0xA2A4);
  writeToReg (0x0C,0x0004);
  writeToReg (0x0D,0x0308);
  writeToReg (0x0E,0x3000);
  mdelay(50);
  writeToReg (0x1E,0x00AF);
  writeToReg (0x01,0x2B3F);
  writeToReg (0x02,0x0600);
  writeToReg (0x10,0x0000);
  writeToReg (0x07,0x0233);
  writeToReg (0x0B,0x0039);
  writeToReg (0x0F,0x0000);
  mdelay(50);

  writeToReg (0x30,0x0707);
  writeToReg (0x31,0x0204);
  writeToReg (0x32,0x0204);
  writeToReg (0x33,0x0502);
  writeToReg (0x34,0x0507);
  writeToReg (0x35,0x0204);
  writeToReg (0x36,0x0204);
  writeToReg (0x37,0x0502);
  writeToReg (0x3A,0x0302);
  writeToReg (0x3B,0x0302);

  writeToReg (0x23,0x0000);
  writeToReg (0x24,0x0000);

  writeToReg (0x48,0x0000);
  writeToReg (0x49,0x013F);
  writeToReg (0x4A,0x0000);
  writeToReg (0x4B,0x0000);

  writeToReg (0x41,0x0000);
  writeToReg (0x42,0x0000);

  writeToReg (0x44,0xEF00);
  writeToReg (0x45,0x0000);
  writeToReg (0x46,0x013F);
  mdelay(50);

  writeToReg (0x44,0xEF00);
  writeToReg (0x45,0x0000);
  writeToReg (0x4E,0x0000);
  writeToReg (0x4F,0x0000);
  writeToReg (0x46,0x013F);
}



void lcd_ctrl_init(tLcdParams* pParams)
{
  tU32 regValue = 0;
  tU32* pPal = 0;
  int i = 0;

  // disable the display
  LCD_CTRL &= ~LCD_CTRL_REG_EN;

  // ppl value to set = ppl / 16 -1 (2:7)
  // hsw value to set = hsw - 1     (8:15)
  // hfp value to set = hfp - 1     (16:23)
  // hbp value to set = hbp - 1     (24:31)

  regValue= ( ((((pParams->ppl/16)-1)&0x3F) << 2)
    |         (( (pParams->hsw-1)    &0xFF) << 8)
    |         (( (pParams->hfp-1)    &0xFF) << 16)
    |         (( (pParams->hbp-1)    &0xFF) << 24) );

  LCD_TIMH = regValue;

  regValue =((((pParams->lpp-1) &0x3FF) << 0)
    |        (((pParams->vsw-1) &0x03F) << 10)
    |        (((pParams->vfp-1) &0x0FF) << 16)
    |        (((pParams->vbp-1) &0x0FF) << 24) );

  LCD_TIMV = regValue;

  regValue = 0;
  if(pParams->acb != 0)
  {
    regValue = (((pParams->acb-1) & 0x1F) << 6);
  }

  if(pParams->ioe != 0)
  {
    regValue |= LCD_POL_REG_IOE;
  }

  if(pParams->ipc != 0)
  {
    regValue |= LCD_POL_REG_IPC;
  }

  if(pParams->ihs != 0)
  {
    regValue |= LCD_POL_REG_IHS;
  }

  if(pParams->ivs != 0)
  {
    regValue |= LCD_POL_REG_IVS;
  }

  // clocks per line (CPL)

  switch(pParams->panelType)
  {
  case LCD_MONO_4:
    regValue |=  ((((pParams->ppl / 4)-1) & 0x3FF) << 16);
    break;
  case LCD_MONO_8:
    regValue |=  ((((pParams->ppl / 8)-1) & 0x3FF) << 16);
    break;
  case LCD_CSTN:
    regValue |= (((((pParams->ppl * 3)/8)-1) & 0x3FF) << 16);
    break;

  case LCD_TFT:
  case LCD_ADTFT:
  case LCD_HRTFT:
  default:
    regValue |= (((pParams->ppl-1) & 0x3FF) << 16);
  }

  // panel clock divisor, lower bits
  regValue |=  ((13)-2)&0x0F;  // TODO: should be calculated from fcclk

  LCD_POL = regValue;

  // skip line end control
  LCD_LE  = 0;

  // disable interrupts
  LCD_INTMSK = 0;

  // default 16 bits per pixel
  regValue = LCD_CTRL_REG_BPP16_565;
  
  // default BGR
  regValue |= LCD_CTRL_REG_BGR;
  
  switch(pParams->panelType)
  {
  case LCD_ADTFT:
  case LCD_HRTFT:
  case LCD_TFT:
    regValue |= LCD_CTRL_REG_TFT;
    break;
  case LCD_MONO_4:
    regValue |= LCD_CTRL_REG_BW_MONO;
    break;
  case LCD_MONO_8:
    regValue |= (LCD_CTRL_REG_MONO8 | LCD_CTRL_REG_BW_MONO);
    break;
  case LCD_CSTN:
    break;
  default:
    // ERROR
    break;
  }

  if(pParams->dp == 1)
  {
    regValue |= LCD_CTRL_REG_DUAL;
  }

  LCD_CTRL = regValue;
  
/*  // clear palette
  
  pPal = (tU32*) LCD_PAL;
  for(i = 0; i < 128; i++)
  {
    *pPal = 0;
    pPal++;
  }
*/
  // 0 = divide by 1
  LCD_CFG = 0x0;

}


/******************************************************************************
 * Implementation of public functions
 *****************************************************************************/



void
lcdSetColorFormat(tColorFormat format)
{
  switch(format)
  {
  case COLOR_FORMAT_RGB:
    LCD_CTRL &= ~LCD_CTRL_REG_BGR; 
    break;
  case COLOR_FORMAT_BGR:
    LCD_CTRL |= LCD_CTRL_REG_BGR;
    break;
  default:
    // ERROR
    break;

  }
}

void
lcdSetBacklight(tU8 percent)
{
  PWM1MR1 = ((unsigned long)0x3000 * (unsigned long)(100-percent)) / (unsigned long)100;
  PWM1LER = 0x02;
}

void
lcdSetBpp(tLcdBpp bpp)
{

  LCD_CTRL &= 0xfffffff1;
  switch (bpp)
  {
  case LCD_BPP1:
    LCD_CTRL |= LCD_CTRL_REG_BPP1;
    break;
  case LCD_BPP2:
    LCD_CTRL |= LCD_CTRL_REG_BPP2;
    break;
  case LCD_BPP4:
    LCD_CTRL |= LCD_CTRL_REG_BPP4;
    break;
  case LCD_BPP8:
    LCD_CTRL |= LCD_CTRL_REG_BPP8;
    break;
  case LCD_BPP16:
    LCD_CTRL |= LCD_CTRL_REG_BPP16;
    break;
  case LCD_BPP24:
    LCD_CTRL |= LCD_CTRL_REG_BPP24;
    break;
  case LCD_BPP16_565:
    LCD_CTRL |= LCD_CTRL_REG_BPP16_565;
    break;    
  case LCD_BPP12_444:
    LCD_CTRL |= LCD_CTRL_REG_BPP12_444;
    break;        
    
  }

}


void
lcdTurnOn(void)
{
  // enable LCD
  LCD_CTRL |= LCD_CTRL_REG_EN;

  // power on
  LCD_CTRL |= LCD_CTRL_REG_PWR;

  mdelay(500);
  lcd_display_init();

  lcdSetBacklight(100);
}

void
lcdInit(tLcdParams* pParams)
{
  lcd_hw_init();
  lcdSetBacklight(0);

  lcd_ctrl_init(pParams);
    
  // set the frame buffer
  LCD_UPBASE = LCD_FRAME_BUFFER;
}

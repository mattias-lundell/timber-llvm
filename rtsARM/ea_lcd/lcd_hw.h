/******************************************************************************
 *
 * Copyright:
 *    (C) 2000 - 2008 Embedded Artists AB
 *
 *****************************************************************************/


#ifndef _LCD__HW_
#define _LCD__HW_

#define tU8   unsigned char
#define tU16  unsigned short
#define tU32  unsigned int
#define tS8   signed char
#define tS16  signed short
#define tS32  int
#define tBool unsigned char
#define TRUE  1
#define FALSE 0


/******************************************************************************
 * Typedefs and defines
 *****************************************************************************/

#define LCD_CTRL_REG_EN        0x01
#define LCD_CTRL_REG_BPP1      (0x00 << 1)
#define LCD_CTRL_REG_BPP2      (0x01 << 1)
#define LCD_CTRL_REG_BPP4      (0x02 << 1)
#define LCD_CTRL_REG_BPP8      (0x03 << 1)
#define LCD_CTRL_REG_BPP16     (0x04 << 1)
#define LCD_CTRL_REG_BPP24     (0x05 << 1)
#define LCD_CTRL_REG_BPP16_565 (0x06 << 1)
#define LCD_CTRL_REG_BPP12_444 (0x07 << 1)
#define LCD_CTRL_REG_BW_MONO   (0x01 << 4) 
#define LCD_CTRL_REG_TFT       (0x01 << 5)
#define LCD_CTRL_REG_MONO8     (0x01 << 6)
#define LCD_CTRL_REG_DUAL      (0x01 << 7)
#define LCD_CTRL_REG_BGR       (0x01 << 8)
#define LCD_CTRL_REG_BEBO      (0x01 << 1)
#define LCD_CTRL_REG_BEPO      (0x01 << 10)
#define LCD_CTRL_REG_PWR       (0x01 << 11)
#define LCD_CTRL_REG__VCOMP_VS (0x00 << 12)
#define LCD_CTRL_REG__VCOMP_BP (0x01 << 12)
#define LCD_CTRL_REG__VCOMP_AV (0x02 << 12)
#define LCD_CTRL_REG__VCOMP_FP (0x03 << 12)
#define LCD_CTRL_REG__WATERM   (0x01 << 16)

// LCD_POL register
#define LCD_POL_REG_IVS (0x01 << 11)
#define LCD_POL_REG_IHS (0x01 << 12)
#define LCD_POL_REG_IPC (0x01 << 13)
#define LCD_POL_REG_IOE (0x01 << 14)


// frame buffer address
#define LCD_FRAME_BUFFER 0xa0000000

typedef enum
{
  LCD_TFT = 0, // standard TFT
  LCD_ADTFT,   // advanced TFT
  LCD_HRTFT,   // highly reflective TFT
  LCD_MONO_4,  // 4-bit mono
  LCD_MONO_8,  // 8-bit mono
  LCD_CSTN     // color STN
} tLcdPanel;

typedef enum
{
  COLOR_FORMAT_RGB = 0,
  COLOR_FORMAT_BGR
} tColorFormat;

typedef enum
{
	LCD_BPP1,
	LCD_BPP2,
	LCD_BPP4,
	LCD_BPP8,
	LCD_BPP16,
	LCD_BPP24,	
	LCD_BPP16_565,
	LCD_BPP12_444	
} tLcdBpp;

/* Structure containing the parameters for the LCD panel */
typedef struct 
{
  // settings for LCD_TIMH

  tU8  hbp; // horizontal back porch
  tU8  hfp; // horizontal fron porch
  tU8  hsw; // horizontal synchronization pulse width
  tU16 ppl; // pixels per line
  
  // settings for LCD_TIMV

  tU8  vbp; // vertical back porch
  tU8  vfp; // vertical front porch
  tU8  vsw; // vertical synchronization pulse width
  tU16 lpp; // lines per panel

  // settings for LCD_POL

  tU8 ioe; // invert out enable
  tU8 ipc; // invert panel clock
  tU8 ihs; // invert horizontal synchronization
  tU8 ivs; // invert vertical synhronization
  tU8 acb; // AC bias pin frequency  

  tU8  bpp; // bits per pixel
  tLcdPanel panelType; // panel type

  tU8 dp; // dual panel
} tLcdParams;



/******************************************************************************
 * Public functions
 *****************************************************************************/

void lcdTurnOn(void);
void lcdInit(tLcdParams* pParams);

#endif

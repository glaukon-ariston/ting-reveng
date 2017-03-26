# Hardware reverse engineering notes

## Ting v1.3

### Identifying data

Generalplus GPEL3101A (embedded ARM7TDMI, the biggest chip on PCB)
    GPEL3101A
    -003B-QL241
    WQHS787.1
    1520

    google: site:www.generalplus.com/doc/
    google: site:www.generalplus.com/doc/ds/
    google: site:www.generalplus.com/doc/an/
    google: g+ code packer generalplus

    Tools
        - G+ Code Packer
            http://www.lcis.com.tw/paper_store/paper_store/UserGuide-2014712113814914-20141242395656.pdf
            http://www.lcis.com.tw/paper_store/paper_store/GPL32XXXV01_Platform Demo Code User(立奕企業股份有限公司)-201471312858961.pdf
        - G+ Easy Writer

    Datasheet
        GPEL3101A - Advanced ELA SoC Solution
        March 12, 2013 Version 0.3
        search.alkon.net/cgi-bin/pdf.pl?pdfname=19859.pdf

    http://www.generalplus.com/product_detail.php?pdv_no=4537&func=dl

    General Description

    The Generalplus GPEL3101A, a highly integrated SoC (System-on-a-Chip), offers a great cost-effective and high performance ratio solution for ELA applications.  It is embedded the ARM7TDMI with 8K-byte unified ID-cache and many tremendous features, SPI Flash controller which CPU can run program directly on it, WMA accelerator, TFT-LCD interface, COMS sensor interface, 60-bit BCH and Randomizer for MLC/TLC NAND Flash, UART interface which support smart card interface(ISO7816), 4-channel DMA controller, 6-channel 16-bit timers, RTC, two SD/MMC card interfaces, USB 2.0 mini-host/device, interrupt controller, SPI (master/slave) controller, 8-channel sound processing unit (SPU), programmable I/O ports, stereo 16-bit DAC for audio playback, 0.5W class AB mono audio amplifier,  8-channel 12-bit ADC, MIC, PLL, I2S TX/RX for external sigma-delta CODEC, 32K-byte OTP with EV mode, power control macro and 136K-byte embedded SRAM when WMA is disable.
    With a complete set of common system peripherals, the GPEL3101A minimizes overall system cost and no additional component needs to be added.  Not only does GPEL3101A feature the high-speed performance, but it is also a cost-effective system and the most importantly - compatible with all ARM based programs.

    Features

    ARM7TDMI CPU with 8KB unified ID-cache, embedded JTAG ICE, and working frequency up to 96MHz.Up to 136KB SRAM for local data buffer when WMA is disable. There is a dedicated 128KB internal SRAM if WMA is enable.32KB OTP with EV modeSPI Flash controller which CPU can run program directly on it. Supports 1-bit/2-bit/4-bit IO mode both on STR and DTR. 2 SPI Flash are allowed in serial or parallel manner.WMA acceleratorVideo-in & COMS sensor interface and CCOR601/CCIR656 standard supportFour-channel DMA controller.TFT-LCD controller.UPS051. (serial RGB)UPS052. (serial RGB dummy)I80 (8-bit system bus) I/F type.CCIR601/CCIR656.Interrupt Controller.Universal Serial Bus (USB) 2.0 high/full speed compliance device and USB mini-host with built-in transceiver. Support Bulk IN/OUT, Audio ISO IN/OUT, Video ISO IN and Interrupt IN transactions.BCH 60-bit/1K and Randomizer for MLT/TLC NAND Flash8-channel sound processing unit (SPU). Each channel in this SPU can do ADPCM/PCM decode, volume multiply and left/right channel mute control.Watchdog timer.Six 16-bit timers/counters.Two SD/ SDHC/ SDIO/ MMC card interfaces.SPI (master/slave) interface with data rate up to 24Mbps.UART (asynchronous serial I/O) interface with baud rate up to 1.8432Mbps and 115.2Kbps. The UART interface can be configured as smart card interface(ISO7816)42 Programmable general I/O ports (GPIO) with pull-high/low control.Built-in Power macro for power on/off controller and a 4.2V to 3.3V LDO and a 3.3V to 1.2V LDO.Dedicated 4.2V to 3.3V LDO for ADC/DAC.Real-time clock (RTC) with independent power supply.216MHz PLL, range from 24MHz to 216MHz with 6MHz step16-bit stereo DAC (2-channel) for audio playback.0.5W class AB mono audio amplifier12-bit ADC with 2 line-in channels and 2 internal channels for battery and 1.2V measurement.MIC with PGA. (Programmable Gain Amplifier)

    I2S TX/RX for external sigma-delta CODEC

8-pin chip (the same side where push buttons are, towards the sensor)
    G
    AH1322
    25Q16BT
    E4R889

10-pin chip (between USB socket and Audio socket)
    R075
    1301

3-pin
    A1SH8
    J3Y

Long narrow one
    CF4444

Flex cable (sensor)
    ZC-GM09
    S2.0

Battery
    EVE 471163
    250mAh 3.7V

### Components

- USB Mini-B Connector
- 12 MHz (or is it 12.9 MHz?) main XTAL
- 4x push buttons (volume up, volume down, power on/of, reset)
- 3.5mm stereo? socket
- MicroSD card socket/holder + 4GB SD card
- Speaker
- 

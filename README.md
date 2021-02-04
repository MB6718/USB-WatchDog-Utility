# USB WatchDog Utility <a name="top"></a>

[![version](https://img.shields.io/badge/version-BETA-BrightGreen)](https://github.com/MB6718/USB-WatchDog-Utility)
[![ide](https://img.shields.io/badge/IDE-Lazarus_1.8.4-ff69b4)](https://www.lazarus-ide.org/)
[![fpc-version](https://img.shields.io/badge/FPC-v3.0.4-blue)](https://www.freepascal.org/download.html)
[![platform](https://img.shields.io/badge/platform-windows-LightGray)](https://ru.wikipedia.org/wiki/%D0%9A%D1%80%D0%BE%D1%81%D1%81%D0%BF%D0%BB%D0%B0%D1%82%D1%84%D0%BE%D1%80%D0%BC%D0%B5%D0%BD%D0%BD%D0%BE%D1%81%D1%82%D1%8C)
[![license](https://img.shields.io/badge/license-GPL_v3.0-yellow)](https://github.com/MB6718/HomeDesk-Solutions/blob/master/LICENSE)

## Contents
* [Short description](#description)
* [Interface](#gui)
* [Compilation](#compile)
* [Protocol specification](#protocolspec)
  * [Protocol description](#protocoldescription)
  * [Protocol commands](#protocolcommand)
    * [cmdChangeTimeOut](#cmdChangeTimeOut)
    * [cmdZero](#cmdzero)
    * [cmdHardReset ](#cmdHardReset)
    * [cmdSoftReset](#cmdsoftreset)
    * [cmdPowerOff](#cmdpoweroff)
    * [cmdSoftMode](#cmdSoftMode)
    * [cmdHardMode](#cmdHardMode)
    * [cmdPowerOffMode](#cmdPowerOffMode)
    * [cmdAccept](#cmdAccept)
    * [cmdHello](#cmdHello)
    * [cmdCheckDevice](#cmdCheckDevice)
    * [cmdGetDeviceVersion](#cmdGetDeviceVersion)
* [Built with](#buildwith)
* [Future features](#futurefeatures)
* [Authors](#authors)
* [Thanks and donations](#donate)
* [License](#license)

## Short description <a name="description"></a>
The USB WatchDog utility is designed to simplify and make the most convenient use of the USB WatchDog timer, of Chinese origin. Also, this utility allows you to use a watchdog timer with custom firmware (MB6718) and advanced functionality. Has an intuitive interface.

The application is implemented using the ObjectPascal (FPC) language and is compiled into a "native" form (executable binary file) for the Windows operating system.

## Interface <a name="gui"></a>
The utility interface is divided by the tab bar method into several sections that group the device and application controls. Below is a list of functions by their sections and groups:

- "Main" tab 
  - The group responsible for configuring the communication port.
    - In this group you can find a color indicator of the port status. Possible indicator colors:  
    <span style="color:Gray">&#10033; gray</span> - no ports found;  
    <span style="color:Crimson">&#10033; red</span> - selected port is not active;  
    <span style="color:Gold">&#10033; yellow</span> - port is active but device is not detected or not recognized;  
    <span style="color:LimeGreen">&#10033; green</span> - everything is fine, the port is active, the device is detected and it is responding.  
    - There is a status bar, this line, like a color indicator, notifies about the connection progress or the current state of the device.
    - A drop-down list showing all ports found in the system.
    - The "Refresh" button searches for new ports and adds them to the list of ports. It also removes ports from the list that were not found in the system. 
    - The "Start/Stop" button opens the port selected from the list and starts the process of polling the device.
    - Display line of the manufacturer and firmware version of the device.
  - The group is responsible for monitoring the "ping" time for a given address. 
    - A switch enabling this function.
    - Internet address input field. The address can be either the IP address of the host, or the DNS name of the host, or a URL string. There is validation of the entered address, invalid address will be rejected and examples of typical addresses will be offered.
    - Slider configure time to wait for a response from a remote node in milliseconds. The minimum value is 1 ms, and the maximum is 2000 ms. (2 sec.). 
    - Indicator that displays the status of the "ping" function. 
  - A group of buttons with basic actions.  
    All buttons are duplicated by the action of pressing the corresponding hotkey. 
    - "Soft Reset" button > F1.
    - "Hard Reset" button > F2.
    - "Power Off" button > F3.
- "Device" tab
  - The group is responsible for the operation of the USB port, included in different power modes. 
    - The USB port is powered from the main power supply and disappears when the system is turned off.
    - The USB port is powered from the "StandBy" power supply and remains "on" after system shutdown .
  - Group for managing the waiting time for a response from the system.
    - Slider configurable waiting time for a response from the system (min: 10 sec., max: 1270 sec. (21 min. 10 sec.)).
    - Quick links to typical time stamps (10 sec., 3 min., 5 min., 10 min., 15 min., 20 min.)
    - Indicator of the set time interval in seconds.
  - A group that switches the mode of processing the "reset" command by the device.
    - "Soft Reset" mode.
    - "Hard Reset" mode.
    - "Power Off" mode. (Available if the appropriate USB power mode is selected)
- "Application" tab  
    This section is responsible for possible application settings.
  - "Running the application in a minimized state"
  - "Run the application together with the OS launch"
  - "Check for updates automatically"
  - "Write event log to file"
  - "Delete (clear) event log" button.
  - "Minimize application on close"
  - "Minimize the application to the system tray"
  - "Mark the current port as the default port"
  - "Automatically connect to device when app starts"
  - "Check for Updates" button.
  - "Restore application settings to default" button.
- "About"  tab
  - Full application name, version and release date.
  - Email address to contact the developer to support the application.
  - Information about the opportunity to thank the author of this utility.
- Bottom information panel
  - The "Help" button, clicking on which leads to an attempt to open the project site with the help section for the utility. 
  - A dynamically link. Appears only if the "Automatically check for updates" option is activated and a fresh update is available. Clicking on this link is equivalent to clicking on the "Verify update" button, after which you will be prompted to automatically download and update the utility.
  - Author's label.
<p align="right"><a href="#top">[ Top ]</a></p>

## Compilation <a name="compile"></a>
Step-by-step instructions for compiling and running the application:
* for Windows
    1. Make sure that the latest (or at least version 1.8.4) version of the Lazarus IDE is installed on the system.
    2. Make sure that the required components and packages from the list of project dependencies have been added to the environment.
    3. Download the archive with the source code and unpack it to any convenient folder or clone the repository to your local machine.
    4. Open the project file `usbwd.lpr`.
    5. If the IDE did not find any errors when opening the project, select the Clean and Build item in the Run menu. After that, the project should successfully compile, which will be evidenced by the corresponding inscription in the IDE message window. But if it doesn't, carefully repeat steps 1 through 5.
    6. After successful compilation of the project, the last step is left, launching the application. Congratulations, the project is assembled and ready to go.
* for Linux (Debian based - Ubuntu and etc.)
    1. While this feature is not available. Porting to this OS is in progress.
<p align="right"><a href="#top">[ Top ]</a></p>

## Protocol specification: <a name="protocolspec"></a>

#### Protocol description <a name="protocoldescription"></a> 

The protocol is quite primitive (simple). Communication is carried out via serial port with parameters 9600 8-N-1. Based on the transfer of one byte of data (command) to the device and receiving a response in the form of one byte from the device. Sequential sending of data (commands) to the device is also allowed. When sending commands sequentially, the device will process them in accordance with the order of receipt. This protocol does not imply control over correct command acceptance and notifying the utility about it.

Any command sent (other than command zero) resets the device's internal timer.

There are two levels of commands received by the device. The first level is the basic level or the "china" level, the commands available for it are marked with a [ $ ] icon. The second level is the level of extended commands to the device with the "MB6718" firmware version, the commands are marked with a [ * ]. The utility automatically detects the level of the command set available.

<p align="right"><a href="#top">[ Top ]</a></p>

#### Protocol commands <a name="protocolcommand"></a> 

- **cmdChangeTimeOut [ $ ]** &rArr; "Change Timeout" command. <a name="cmdChangeTimeOut"></a>  
  Request: `0x01..0x7F`  
  Response: `0xAA`  
  Device action: Remember a new time period.  
  Detailed command description: Integer range of values from min. 1 to max. 127 in decimal notation. Each unit is multiplied by 10 (multiplicity) and corresponds to the time in seconds. Time encoding example: 0x2F > 47d * 10d > 470 sec. That is, if we need to send the device a timeout equal for example 5 minutes, then 5 minutes = 300 seconds / 10 (multiplicity) = 30d > 0x1E.

<p align="right"><a href="#top">[ Top ]</a></p>

- **cmdZero [ $ ]** &rArr; "Null" command. <a name="cmdzero"></a>  
  Request: `0x00`  
  Response: Nothin.   
  Device action: Nothin.  
  Detailed command description: This command is completely ignored by the device.

<p align="right"><a href="#top">[ Top ]</a></p>

- **cmdHardReset [ $ ]** &rArr; "Hard reset" command. <a name="cmdHardReset"></a>  
  Request: `0xFE`  
  Response: `0xAA`  
  Device action: Forcing a "hard reset" procedure.  
  Detailed command description: Upon receiving this command, the device immediately performs a so-called "hard" reset.

<p align="right"><a href="#top">[ Top ]</a></p>

- **cmdSoftReset [ $ ]** &rArr; "Soft reset" command. <a name="cmdsoftreset"></a>  
  Request: `0xFF`  
  Response: `0xAA`  
  Device action: Forcing a "soft reset" procedure.  
  Detailed command description: Upon receiving this command, the device immediately performs a so-called "soft" reset.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdPowerOff [ * ]__ &rArr; "Power off" command. <a name="cmdpoweroff"></a>  
  Request: `0xFD`  
  Response: `0xAA`  
  Device action: Forcing a "power off" procedure.  
  Detailed command description: Upon receiving this command, the device immediately performs a so-called "power off".

  > **<span style="color:#FF9900">Attention!</span>** Works only if USB power supply from VSB (StandBy). Otherwise, it is no different from the "cmdHardReset" command.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdSoftMode [ * ]__ &rArr; Command for switching the operating mode to the "soft reset" mode. <a name="cmdSoftMode"></a>  
  Request: `0xA0`  
  Response: `0xAA`  
  Device action: Change of the operating mode.  
  Detailed command description: By executing this command, the device will switch the internal timer triggering mode to the "soft reset" mode. This means that if the signal from the system disappears (the system hangs), the device while in this mode will perform a "soft" reset, similar to the "cmdSoftReset" command.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdHardMode [ * ]__ &rArr; Command to transfer the operating mode to the "hard reset" mode. <a name="cmdHardMode"></a>  
  Request: `0xA1`  
  Response: `0xAA`  
  Device action: Change of the operating mode.  
  Detailed command description: By executing this command, the device will switch the internal timer activation mode to the "hard reset" mode. This means that if the signal from the system is lost (the system hangs), the device being in this mode will perform a "hard" reset, similar to the "cmdHardReset" command.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdPowerOffMode [ * ]__ &rArr; Command to transfer the operating mode to the "power off" mode. <a name="cmdPowerOffMode"></a>  
  Request: `0xA2`  
  Response: `0xAA`  
  Device action: Change of the operating mode.  
  Detailed command description: By executing this command, the device will switch the internal timer activation mode to the "off" mode. This means that if the signal from the system is lost (the system hangs), the device being in this mode will "shutdown" the system, similar to the command "cmdPowerOff".

  > **<span style="color:#FF9900">Attention!</span>** Works only if USB power supply from VSB (StandBy). Otherwise, it is no different from the "cmdHardMode" mode.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdAccept [ * ]__ &rArr; "Accepted" command. <a name="cmdAccept"></a>  
  Request: `0xAA`  
  Response: `0xAA`  
  Device action: Return the given command back.  
  Detailed command description: The main purpose of this command is to confirm the receipt and execution of any command by the device. To send this command, the device will respond with the same command. It does not affect the operation of the device and therefore there is no practical sense in sending this command to the device.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdHello [ $ ]__ &rArr; "Hello" command. <a name="cmdHello"></a>  
  Request: `0x80`  
  Response: `0x81`  
  Device action: Returning a response about the readiness of the device in the form of a command "Check device"  
  Detailed command description: This command is used to determine the device and its readiness for operation. Participates in organizing the "handshake" mode.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdCheckDevice [ * ]__ &rArr; "Check device" command. <a name="cmdCheckDevice"></a>  
  Request: `0x81`  
  Response: `0x80`  
  Device action: Returns the "Hello" response.  
  Detailed command description: By analyzing the result of this command, you can determine the manufacturer of the firmware and the level of available commands. If the answer is 0x80, the device supports extended command set. If the answer is 0x82 or other than 0x80, the device supports the basic command set. It can be used in conjunction with the "cmdHello" command to organize the "handshake" mode.

<p align="right"><a href="#top">[ Top ]</a></p>

- __cmdGetDeviceVersion [ * ]__ &rArr; "Get Device Version" command. <a name="cmdGetDeviceVersion"></a>  
  Request: `0x88`  
  Response: `0x01..0x7F`  
  Device action: Response with the firmware version.  
  Detailed command description: This command queries the device for its firmware version. The answer is a single-byte integer, which can be converted to obtain the firmware version. The algorithm for obtaining the version is as follows, for example, having received the number 0x1F, we select the high and low nibbles 1 and F. The high nibble is the major version, respectively, the low nibble is the minor version. Converting the hexadecimal representation of the number to decimal, we get version 1.16.

  > **<span style="color:#0066FF">Information!</span>** This versioning method has a number of natural limitations and disadvantages, but it fits well into the current protocol. The maximum version is limited to 7.16 and if we use each version and sub-version, we get 127 possible combinations.

<p align="right"><a href="#top">[ Top ]</a></p>

## Future features <a name="futurefeatures"></a>

Since the development of significant functionality of the utility is inextricably linked with its development in the firmware of the device, new capabilities depending on this will be added taking into account their addition to the firmware. Other features not directly related to hardware or firmware will be added as they become available.

Chips, ideas and features that the goal is to implement in the next versions of the utility (the list is in constant development and some items can be removed or added when the status of their relevance changes):

- [x] Backward compatibility with the device (with firmware) from the Chinese fabrication
- [x] Automatic detection of device firmware version and type
- [ ] Port the project to Linux OS based on Debian distribution (Debian, Ubuntu, Kubuntu, Mint, ...)
- [x] Ping time monitoring for WEB or DNS address
- [ ] Ability to select several *NET addresses from the list to monitor them
- [ ] Process monitoring
- [ ] Multilingual utility interface
- [x] Ability to automatically check for new versions and update the utility
<p align="right"><a href="#top">[ Top ]</a></p>

## Build with <a name="buildwith"></a>

* [FPC 3.0.2](https://www.freepascal.org/) - ObjectPascal compiler version 3.0.2 or higher
* [IDE Lazarus 1.8.4](https://www.lazarus-ide.org/) - Lazarus IDE 1.8.4 or higher

Other dependent modules and packages used in the project:

- [FCL 1.0.1](https://wiki.freepascal.org/FCL/) - version 1.0.1 and higher
- [LCL 1.8.4](https://wiki.freepascal.org/LCL) - version 1.8.4 and higher
- [Synapse 40.1 (laz_synapse)](http://www.ararat.cz/synapse/doku.php/start) - version 40.1 and higher
- [LazSerialPort 0.2](https://github.com/JurassicPork/TLazSerial) - version 0.2 and higher
- [Log4Pascal](https://github.com/martinusso/log4pascal) - Adapted version (original from the module author's link)
<p align="right"><a href="#top">[ Top ]</a></p>

## Authors <a name="authors"></a>

Idea and all code:
* <img src="https://avatars2.githubusercontent.com/u/61043468?s=400&v=4" width="24" height="24"/> [__Max [MB6718] Bee__](https://github.com/MB6718)
<p align="right"><a href="#top">[ Top ]</a></p>

## Thanks and donations <a name="donate"></a>

If you like this utility, you want to thank the author or you want to take part in the development of the project, there are several ways for you to choose from (__any way of gratitude is important for the project and will help its development__):

1. For monetary gratitude, you can send any number of coins to the following addresses to choose from:

   **Monero** (XMR) wallet address: [42u7Gj1tUgRBo2V6SqcvyBdrF1mTN1rU62LcHFJdvYYr4vtwCxck5HdeMwfPWzLj7w2i6PsX2h64gfP5b84vWLceLdZyimg]()

   **Ethereum** (ETH) wallet address: [0x044e4ba3369716158a67f1138cfc84984fb9fd2d]()

   **Bitcoin** (BTC) wallet address: [3CYsMhTT1qVvRXgJ6gc7kk3NiRnFxjCEJr]()

2. To join the team and participate in development, just email me, where I can discuss joining with you:

   e-mail: mb6718@mail.ru

3. For new ideas and just words of gratitude, my email address is above for you.

   
   
   Thank you all and good luck! See you in next projects.
<p align="right"><a href="#top">[ Top ]</a></p>

## License <a name="license"></a>

USB WatchDog Utility is licensed under the GNU GPL v3.0 license. See [LICENSE](https://github.com/MB6718/USB-WatchDog-Utility/blob/main/LICENSE) for more complete details.

&copy; 2020 Designed by MB6718
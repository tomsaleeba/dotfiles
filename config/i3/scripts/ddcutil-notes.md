https://askubuntu.com/a/1181157/234373
https://manpages.ubuntu.com/manpages/focal/man1/ddcutil.1.html

# Get brightness
`sudo ddcutil --display 1 getvcp 10`

# Set brightness
`sudo ddcutil --display 1 setvcp 10 30`

sudo ddcutil probe

I think `EA` is the flag for BI:
```
$ sudo ddcutil --display 1 getvcp EA # on
VCP code 0xea (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
$ sudo ddcutil --display 1 getvcp EA # off
VCP code 0xea (Manufacturer Specific         ): mh=0x00, ml=0x05, sh=0x00, sl=0x00
```
See if I can get all settings (brightness, etc) matching between on and off,
then I'll be sure this is the only one that changes.


```
$ for i in EA EB EC; do sudo ddcutil --display 1 getvcp $i; done # all off
VCP code 0xea (Manufacturer Specific         ): mh=0x00, ml=0x05, sh=0x00, sl=0x00
VCP code 0xeb (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xec (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
$ for i in EA EB EC; do sudo ddcutil --display 1 getvcp $i; done # hdr only
VCP code 0xea (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xeb (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xec (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
$ for i in EA EB EC; do sudo ddcutil --display 1 getvcp $i; done # both on
VCP code 0xea (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xeb (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xec (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
$ for i in EA EB EC; do sudo ddcutil --display 1 getvcp $i; done # bi only
VCP code 0xea (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xeb (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
VCP code 0xec (Manufacturer Specific         ): mh=0xff, ml=0xff, sh=0x02, sl=0x02
```

BI also affects 0xEA


# Change input
```
$ sudo ddcutil probe
...
   Feature: 60 (Input Source)
      Values (unparsed): 0F 10 11 12
      Values (  parsed):
         0f: DisplayPort-1
         10: DisplayPort-2
         11: HDMI-1
         12: HDMI-2
```

Get the current input:
```
$ sudo ddcutil --display 1 getvcp 60
VCP code 0x60 (Input Source                  ): DisplayPort-1 (sl=0x0f)
```

Set the input to the Mac. It's all messed up because the OSD shows `HDMI1` but
it's actually `HDMI-2` for this script:
```
sudo ddcutil --display 1 setvcp 60 0x12
```

Set input to the Lenovo:
```
sudo ddcutil --display 1 setvcp 60 0x0f
```

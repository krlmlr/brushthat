# Creating the GIF

```
mplayer -ao null out.ogv -vo jpeg:outdir=i/tmp/ogv_to_gif
convert -resize 50% -crop 399x533+8+0 +repage $(ls *.jpg | sed -n "0~3p") \
  -loop 0 -set delay 25 -fuzz 5% -layers Optimize +map xx.gif
gifsicle --colors 16 -O2 xx.gif -o demo.gif
```


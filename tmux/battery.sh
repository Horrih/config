#!/bin/bash
percentage=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep 'percentage')
if ! (echo $percentage | grep -q "should be ignored"); then
    echo " $(echo $percentage | sed 's/[^0-9]*//') % |"
fi


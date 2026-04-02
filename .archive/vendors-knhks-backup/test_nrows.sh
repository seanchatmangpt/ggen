#!/bin/bash
echo "Testing maximum NROWS for 2ns (8 ticks) target..."
echo ""

for nrows in 1 2 4 8 16 32 64 128 256; do
    echo -n "NROWS=$nrows: "
    clang -O3 -march=armv8.5-a+fp16 -std=c11 -DNROWS=$nrows -I/opt/homebrew/Cellar/raptor/2.0.16/include knhk_8tick_poc.c -o knhk_8tick_poc -L/opt/homebrew/Cellar/raptor/2.0.16/lib -lraptor2 2>/dev/null
    
    if [ $? -eq 0 ]; then
        avg=$(for i in {1..10}; do ./knhk_8tick_poc 2>&1 | grep "ASK" | awk '{print $3}'; done | awk '{sum+=$1; count++} END {if(count>0) print sum/count}')
        ticks=$(echo "$avg / 0.25" | bc -l)
        printf "%.3f ns/op (%.1f ticks) " $avg $ticks
        if (( $(echo "$avg < 2.0" | bc -l) )); then
            echo "✅ UNDER 2ns"
        else
            echo "❌ OVER 2ns"
        fi
    else
        echo "Compile failed"
    fi
done

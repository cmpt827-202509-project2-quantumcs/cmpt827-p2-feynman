#!/usr/bin/bash
if [ ! \( -f Feynman.cabal \) -a \( -d ../cmpt827-tests \) -a \( -f ../Feynman.cabal \) ]; then
    cd ..;
elif [ \( ! -d cmpt827-tests \) -o \( ! -f Feynman.cabal \) ]; then
    echo "Not in Feynman root folder? Couldn't locate tests folder or Feynman.cabal."
    exit 1
fi
echo "@@> CMPT 827 benchmarks:"
echo "@@>  Building Feynman.."
cabal build
FEYNOPT=(dist-newstyle/build/*/*/Feynman-0.1.0.0/x/feynopt/build/feynopt/feynopt)
FEYNVER=(dist-newstyle/build/*/*/Feynman-0.1.0.0/x/feynver/build/feynver/feynver)
if [ ! -f "$FEYNOPT" ]; then
    echo "Couldn't find feynopt after building?"
    exit 1
fi
BENCHMARKS=(
#   barenco_tof_10
    barenco_tof_3
#   barenco_tof_4
#   barenco_tof_5
#   csla_mux_3
#   csum_mux_9
#   fprenorm
#  'gf2^10_mult'
#  'gf2^4_mult'
#  'gf2^5_mult'
#  'gf2^6_mult'
#  'gf2^7_mult'
#  'gf2^8_mult'
#  'gf2^9_mult'
#   grover_5
#   ham15-low
#   hwb6
#   mod5_4
#   mod_mult_55
#   mod_red_21
#   qcla_adder_10
#   qcla_com_7
#   qcla_mod_7
#   qft_4
#   rc_adder_6
#   tof_10
    tof_3
#   tof_4
#   tof_5
#   vbe_adder_3
)
echo ${BENCHMARKS[@]}
# adder_8.qc
# cycle_17_3.qc
#'gf2^128_mult.qc'
#'gf2^16_mult.qc'
#'gf2^256_mult.qc'
#'gf2^32_mult.qc'
#'gf2^64_mult.qc'
# ham15-high.qc
# ham15-med.qc
# hwb10.qc
# hwb11.qc
# hwb12.qc
# hwb8.qc
# mod_adder_1024.qc
# mod_adder_1048576.qc
for X in ${BENCHMARKS[@]}; do
    echo "@@>  Testing performance over $X.."
    echo "@@   Reference path sum: "`"$FEYNVER" benchmarks/qc/$X.qc`

    /usr/bin/time -o cmpt827-tests/out/bench-time-base-$X.txt \
        "$FEYNOPT" --ftr-trace-astar -cnotmin benchmarks/qc/$X.qc \
            > cmpt827-tests/out/bench-out-base-$X.qc \
            2> cmpt827-tests/out/bench-err-base-$X.txt
    echo "@@   graysynth path sum: "`"$FEYNVER" benchmarks/qc/$X.qc`

    # /usr/bin/time -o cmpt827-tests/out/bench-time-astar-t-$X.txt \
    #     "$FEYNOPT" --ftr-trace-astar --ftr-gas-heuristic-trivial -cnotminGrAStar benchmarks/qc/$X.qc \
    #         > cmpt827-tests/out/bench-out-astar-t-$X.qc \
    #         2> cmpt827-tests/out/bench-err-astar-t-$X.txt
    # echo "@@   graysynth path sum: "`"$FEYNVER" cmpt827-tests/out/bench-out-astar-t-$X.qc`

    /usr/bin/time -o cmpt827-tests/out/bench-time-astar-pc-$X.txt \
        "$FEYNOPT" --ftr-trace-astar --ftr-gas-heuristic-phasecount -cnotminGrAStar benchmarks/qc/$X.qc \
            > cmpt827-tests/out/bench-out-astar-pc-$X.qc \
            2> cmpt827-tests/out/bench-err-astar-pc-$X.txt
    echo "@@   grastar path sum: "`"$FEYNVER" cmpt827-tests/out/bench-out-astar-pc-$X.qc`

done
echo "@@> Done."

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
#   barenco_tof_10.qc
#   barenco_tof_3.qc
  barenco_tof_4.qc
#   barenco_tof_5.qc
#   csla_mux_3.qc
#   csum_mux_9.qc
#   fprenorm.qc
#  'gf2^10_mult.qc'
#  'gf2^4_mult.qc'
#  'gf2^5_mult.qc'
#  'gf2^6_mult.qc'
#  'gf2^7_mult.qc'
#  'gf2^8_mult.qc'
#  'gf2^9_mult.qc'
#   grover_5.qc
#   ham15-low.qc
#   hwb6.qc
#   mod5_4.qc
#   mod_mult_55.qc
#   mod_red_21.qc
#   qcla_adder_10.qc
#   qcla_com_7.qc
#   qcla_mod_7.qc
#   qft_4.qc
#   rc_adder_6.qc
#   tof_10.qc
#   tof_3.qc
#   tof_4.qc
#   tof_5.qc
#   vbe_adder_3.qc
)
echo $BENCHMARKS
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
for X in $BENCHMARKS; do
    echo "@@>  Testing performance over $X.."
    echo "@@   Reference path sum: "`"$FEYNVER" benchmarks/qc/$X`

    /usr/bin/time -o cmpt827-tests/out/bench-time-base-$X.txt \
        "$FEYNOPT" --ftr-trace-astar -cnotmin benchmarks/qc/$X \
            > cmpt827-tests/out/bench-out-base-$X.txt \
            2> cmpt827-tests/out/bench-err-base-$X.txt
    echo "@@   graysynth path sum: "`"$FEYNVER" benchmarks/qc/$X`

    # /usr/bin/time -o cmpt827-tests/out/bench-time-astar-t-$X.txt \
    #     "$FEYNOPT" --ftr-trace-astar --ftr-gas-heuristic-trivial -cnotminGrAStar benchmarks/qc/$X \
    #         > cmpt827-tests/out/bench-out-astar-t-$X \
    #         2> cmpt827-tests/out/bench-err-astar-t-$X.txt
    # echo "@@   graysynth path sum: "`"$FEYNVER" cmpt827-tests/out/bench-out-astar-t-$X`

    /usr/bin/time -o cmpt827-tests/out/bench-time-astar-pc-$X.txt \
        "$FEYNOPT" --ftr-trace-astar --ftr-gas-heuristic-phasecount -cnotminGrAStar benchmarks/qc/$X \
            > cmpt827-tests/out/bench-out-astar-pc-$X \
            2> cmpt827-tests/out/bench-err-astar-pc-$X.txt
    echo "@@   grastar path sum: "`"$FEYNVER" cmpt827-tests/out/bench-out-astar-pc-$X`

done
echo "@@> Done."

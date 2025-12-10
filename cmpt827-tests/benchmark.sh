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
OUT_DIR="cmpt827-tests/out"
TIMEOUT=10
BENCHMARKS=(
    barenco_tof_10
    barenco_tof_3
    barenco_tof_4
    barenco_tof_5
    csla_mux_3
    csum_mux_9
    fprenorm
   'gf2^10_mult'
   'gf2^4_mult'
   'gf2^5_mult'
   'gf2^6_mult'
   'gf2^7_mult'
   'gf2^8_mult'
   'gf2^9_mult'
    grover_5
    ham15-low
    hwb6
    mod5_4
    mod_mult_55
    mod_red_21
    qcla_adder_10
    qcla_com_7
    qcla_mod_7
    qft_4
    rc_adder_6
    tof_10
    tof_3
    tof_4
    tof_5
    vbe_adder_3

    adder_8.qc
    cycle_17_3.qc
   'gf2^128_mult.qc'
   'gf2^16_mult.qc'
   'gf2^256_mult.qc'
   'gf2^32_mult.qc'
   'gf2^64_mult.qc'
    ham15-high.qc
    ham15-med.qc
    hwb10.qc
    hwb11.qc
    hwb12.qc
    hwb8.qc
    mod_adder_1024.qc
    mod_adder_1048576.qc
)


function benchmark_one {
    NAME="$1"
    ARGS="$2"
    QC_NAME="$3"
    QC_IN="$4"

    echo
    echo "@@>  Testing ${NAME}:"
    SECONDS=0
    QC_OUT="${OUT_DIR}/${NAME}_${QC_NAME}.qc"
    /usr/bin/time -o ${OUT_DIR}/${NAME}_${QC_NAME}_time.txt \
        timeout "${TIMEOUT}" "${FEYNOPT}" ${ARGS} "${QC_IN}" \
            > "${QC_OUT}" 2> "${OUT_DIR}/${NAME}_${QC_NAME}.txt"
    echo "@@    Approximately ${SECONDS} seconds."
    echo "@@    ${NAME} path sum: "`"${FEYNVER}" "${QC_OUT}"`
    echo "@@    ${NAME} verify: "`"${FEYNVER}" "${QC_IN}" "${QC_OUT}"`
}

function benchmark {
    QC_NAME="$1"
    QC_IN="$2"

    if [ -z "$QC_IN" ]; then
        QC_IN="benchmarks/qc/$QC_NAME.qc"
    fi

    echo
    echo "@@> Testing performance over $QC_NAME.."
    echo "@@   Reference path sum: "`"${FEYNVER}" "${QC_IN}"`

    benchmark_one graysynth "-cnotmin" "${QC_NAME}" "${QC_IN}"
    benchmark_one grastar-trivial "--ftr-trace-astar --ftr-gas-heuristic-trivial -cnotminGrAStar" "${QC_NAME}" "${QC_IN}"
    benchmark_one grastar-phasecount "--ftr-trace-astar --ftr-gas-heuristic-phasecount -cnotminGrAStar" "${QC_NAME}" "${QC_IN}"
}


for X in ${BENCHMARKS[@]}; do
    benchmark "${X}"
done

echo
echo "@@> Done."

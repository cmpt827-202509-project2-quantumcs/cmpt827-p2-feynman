#!/usr/bin/bash
if [ ! \( -f Feynman.cabal \) -a \( -d ../cmpt827-tests \) -a \( -f ../Feynman.cabal \) ]; then
    cd ..;
elif [ \( ! -d cmpt827-tests \) -o \( ! -f Feynman.cabal \) ]; then
    printf "Not in Feynman root folder? Couldn't locate tests folder or Feynman.cabal.\n"
    exit 1
fi

OUT_DIR="cmpt827-tests/out/"`date +%m%d-%H%M%S`
LOG="${OUT_DIR}/benchmark.log"

TIMEOUT=60

BENCHMARKS=(
# Quick
    barenco_tof_3
    barenco_tof_4
    qft_4
    tof_3
    tof_4

# Tolerably slow
#     hwb6
#     mod5_4
#     mod_mult_55
#     tof_5

# Too slow for regular use, you may need to increase timeout
#     grover_5

# Too slow period but I have some hope
#     barenco_tof_5
#     barenco_tof_10
#     csla_mux_3
#     csum_mux_9
#     fprenorm
#    'gf2^4_mult'

# Probably won't ever be quick enough
#    'gf2^5_mult'
#    'gf2^6_mult'
#    'gf2^7_mult'
#    'gf2^8_mult'
#    'gf2^9_mult'
#    'gf2^10_mult'
#     ham15-low
#     mod_red_21
#     qcla_adder_10
#     qcla_com_7
#     qcla_mod_7
#     rc_adder_6
#     tof_10
#     vbe_adder_3

# Some of these we can't even display the path sum
#     adder_8
#     cycle_17_3
#    'gf2^128_mult'
#    'gf2^16_mult'
#    'gf2^256_mult'
#    'gf2^32_mult'
#    'gf2^64_mult'
#     ham15-high
#     ham15-med
#     hwb10
#     hwb11
#     hwb12
#     hwb8
#     mod_adder_1024
#     mod_adder_1048576
)


mkdir -p "${OUT_DIR}"

FEYNOPT=(dist-newstyle/build/*/*/Feynman-0.1.0.0/x/feynopt/build/feynopt/feynopt)
FEYNVER=(dist-newstyle/build/*/*/Feynman-0.1.0.0/x/feynver/build/feynver/feynver)
if [ ! -f "${FEYNOPT}" ]; then
    printf "Couldn't find feynopt after building?\n"
    exit 1
fi


function benchmark_one {
    NAME="$1"
    ARGS="$2"
    QC_NAME="$3"
    QC_IN="$4"

    printf "\n"
    printf "@@>  Testing %s over %s:\n" "${NAME}" "${QC_NAME}"
    SECONDS=0
    QC_OUT="${OUT_DIR}/${NAME}_${QC_NAME}.qc"
    /usr/bin/time -o ${OUT_DIR}/${NAME}_${QC_NAME}_time.txt \
        timeout "${TIMEOUT}" "${FEYNOPT}" ${ARGS} "${QC_IN}" \
            > "${QC_OUT}" 2> "${OUT_DIR}/${NAME}_${QC_NAME}.log"
    printf "@@    Finished in ~%s seconds.\n" ${SECONDS}
    printf "@@    %s path sum: %s\n" ${NAME} "$("${FEYNVER}" "${QC_OUT}")"
    printf "@@    %s verify: %s\n" ${NAME} "$("${FEYNVER}" "${QC_IN}" "${QC_OUT}")"
}


function benchmark {
    QC_NAME="$1"
    QC_IN="$2"

    if [ -z "${QC_IN}" ]; then
        QC_IN="benchmarks/qc/${QC_NAME}.qc"
    fi

    printf "\n"
    printf "@@> Testing performance over %s..\n" "${QC_NAME}"
    printf "@@   Reference path sum: %s\n" "$("${FEYNVER}" "${QC_IN}")"

    benchmark_one graysynth "-cnotmin" "${QC_NAME}" "${QC_IN}"
    benchmark_one grastar-trivial "--ftr-trace-astar --ftr-gas-heuristic-trivial -cnotminGrAStar" "${QC_NAME}" "${QC_IN}"
    benchmark_one grastar-phasecount "--ftr-trace-astar --ftr-gas-heuristic-phasecount -cnotminGrAStar" "${QC_NAME}" "${QC_IN}"
}


function run_all {
    printf "@@> CMPT 827 benchmarks:\n"
    printf "@@>  Building Feynman..\n"
    cabal build
    for X in ${BENCHMARKS[@]}; do
        benchmark "${X}"
    done

    printf "\n"
    printf "@@> Done.\n"
}


run_all | tee -a ${LOG}

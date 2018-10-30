
(* This file was auto-generated based on "sintatico.msg". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "\"program\" esperado\n"
    | 1 ->
        "<id> esperado\n"
    | 2 ->
        "\";\" esperado\n"
    | 3 ->
        "\"begin\" esperado\n"
    | 4 ->
        "<id [, ...id'] : tipo> esperado\n"
    | 5 ->
        "<[, ...id'] : tipo> esperado\n"
    | 6 ->
        "<id [, ...id] : tipo> esperado\n"
    | 11 ->
        "<tipo> esperado\n"
    | 8 ->
        "\";\" esperado\n"
    | 9 ->
        "\"begin\" esperado\n"
    | 22 ->
        "<id> esperado\n"
    | 23 ->
        "\"(\" esperado\n"
    | 24 ->
        "\")\" esperado\n"
    | 26 ->
        "<tipo> esperado\n"
    | 28 ->
        "\")\" esperado\n"
    | 29 ->
        "<: tipo> esperado\n"
    | 30 ->
        "<tipo> esperado\n"
    | 31 ->
        "\";\" esperado\n"
    | 32 ->
        "\"begin\" esperado\n"
    | 157 ->
        "\";\" esperado\n"
    | 155 ->
        "\"begin\" esperado\n"
    | 36 ->
        "\"end\" esperado\n"
    | 37 ->
        "<express\195\163o> esperada\n"
    | 85 ->
        "\"do\" esperado\n"
    | 38 ->
        "<express\195\163o> esperada\n"
    | 84 ->
        "\"do\" esperado\n"
    | 86 ->
        "\"do\" esperado\n"
    | 51 ->
        "<expresss\195\163o> esperada\n"
    | 52 ->
        "\"do\" esperado\n"
    | 53 ->
        "<express\195\163o> esperada\n"
    | 55 ->
        "<express\195\163o> esperada\n"
    | 57 ->
        "<express\195\163o> esperada\n"
    | 58 ->
        "\"do\" esperado\n"
    | 61 ->
        "<express\195\163o> esperada\n"
    | 62 ->
        "\"do\" esperado\n"
    | 65 ->
        "<express\195\163o> esperada\n"
    | 66 ->
        "\"do\" esperado\n"
    | 63 ->
        "<express\195\163o> esperada\n"
    | 64 ->
        "\"do\" esperado\n"
    | 67 ->
        "<express\195\163o> esperada\n"
    | 68 ->
        "\"do\" esperado\n"
    | 69 ->
        "<express\195\163o> esperada\n"
    | 70 ->
        "\"do\" esperado\n"
    | 71 ->
        "<express\195\163o> esperada\n"
    | 72 ->
        "\"do\" esperado\n"
    | 73 ->
        "<express\195\163o> esperada\n"
    | 74 ->
        "\"do\" esperado\n"
    | 87 ->
        "\"begin\" esperado\n"
    | 59 ->
        "<express\195\163o> esperada\n"
    | 75 ->
        "<express\195\163o> esperada\n"
    | 76 ->
        "\"do\" esperado\n"
    | 46 ->
        "<express\195\163o> esperada\n"
    | 45 ->
        "<express\195\163o> esperada\n"
    | 50 ->
        "\")\" esperado\n"
    | 35 ->
        "\"end\" esperado\n"
    | 89 ->
        "\"(\" esperado\n"
    | 90 ->
        "<express\195\163o> esperada\n"
    | 81 ->
        "\")\" esperado\n"
    | 82 ->
        "<express\195\163o> esperada\n"
    | 93 ->
        "\"(\" esperado\n"
    | 94 ->
        "<express\195\163o> esperada\n"
    | 97 ->
        "<express\195\163o> esperada\n"
    | 98 ->
        "\"then\" esperado\n"
    | 99 ->
        "\"begin\" esperado\n"
    | 100 ->
        "\";\" esperado\n"
    | 101 ->
        "\"begin\" esperado\n"
    | 43 ->
        "\":=\" esperado\n"
    | 134 ->
        "<id> esperado\n"
    | 47 ->
        "<id> esperado\n"
    | 135 ->
        "<express\195\163o> esperada\n"
    | 136 ->
        "\";\" esperado\n"
    | 44 ->
        "<express\195\163o> esperada\n"
    | 148 ->
        "\";\" esperado\n"
    | 149 ->
        "\"end\" esperado;\n"
    | 104 ->
        "<express\195\163o> esperada\n"
    | 106 ->
        "\":=\" esperado\n"
    | 107 ->
        "<express\195\163o> esperada\n"
    | 108 ->
        "\"to\" esperado\n"
    | 109 ->
        "<express\195\163o> esperada\n"
    | 110 ->
        "\"do\" esperado\n"
    | 111 ->
        "\"begin\" esperado\n"
    | 113 ->
        "\"(\" esperado\n"
    | 114 ->
        "<express\195\163o> esperada\n"
    | 117 ->
        "\"(\" esperado\n"
    | 118 ->
        "<express\195\163o> esperada\n"
    | 159 ->
        "\".\" esperado\n"
    | 160 ->
        "\"eof\" esperado\n"
    | 121 ->
        "<variavel> esperada\n"
    | 122 ->
        "\"of\" esperado\n"
    | 123 ->
        "<caso> esperado\n"
    | 130 ->
        "\":\" esperado\n"
    | 131 ->
        "<express\195\163o> esperada\n"
    | 137 ->
        "<express\195\163o> esperada\n"
    | 145 ->
        "\"end\" esperado\n"
    | 125 ->
        "\"begin\" esperado\n"
    | 126 ->
        "\";\" esperado\n"
    | 128 ->
        "\"end\" esperado\n"
    | _ ->
        raise Not_found

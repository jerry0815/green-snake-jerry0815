mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: input_equal,
        file: "input_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: add,
        file: "add.snek",
        expected: "15",
    },
    {
        name: nested_arith,
        file: "nested_arith.snek",
        input: "8",
        expected: "1117",
    },
    {
        name: binding,
        file: "binding.snek",
        expected: "5",
    },
    {
        name: bind2,
        file: "bind2.snek",
        expected: "6",
    },
    {
        name: nested_let,
        file: "nested_let.snek",
        expected: "12",
    },
    {
        name: mul_same_var_let,
        file: "mul_same_var_let.snek",
        expected: "21",
    },
    {
        name: mul_var_bound,
        file: "mul_var_bound.snek",
        expected: "3",
    },
    {
        name: complex_nest,
        file: "complex_nest.snek",
        expected: "-90",
    },
    {
        name: integrate,
        file: "integrate.snek",
        expected: "111",
    },
    {
        name: sub_integrate,
        file: "sub_integrate.snek",
        expected: "100",
    },
    {
        name: sub_integrate2,
        file: "sub_integrate2.snek",
        expected: "10",
    },
    {
        name: factorial1,
        file: "factorial.snek",
        input: "3",
        expected: "6",
    },
    {
        name: factorial2,
        file: "factorial.snek",
        input: "5",
        expected: "120",
    },
    {
        name: let_block,
        file: "let_block.snek",
        expected: "6",
    },
    {
        name: integrate_cobra,
        file: "integrate_cobra.snek",
        expected: "-6",
    }
    ,
    {
        name: overflow_1,
        file: "overflow_1.snek",
        expected: "-4611686018427387904",
    }
    ,
    {
        name: overflow_2,
        file: "overflow_2.snek",
        expected: "4611686018427387903",
    },
    {
        name: overflow_4,
        file: "overflow_4.snek",
        expected: "-4611686018427387904",
    },
    {
        name: overflow_5,
        file: "overflow_5.snek",
        expected: "-10",
    },
    {
        name: complex_expr,
        file: "complex_expr.snek",
        expected: "4611686018427387902",
    },
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: double,
        file: "double.snek",
        expected: "4",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: sumrec,
        file: "sumrec.snek",
        expected: "6",
    },
    {
        name: simple_iseven,
        file: "simple_iseven.snek",
        expected: "true",
    },
    {
        name: print_test,
        file: "print_test.snek",
        input: "10",
        expected: "10\n15\n15",
    },
    {
        name: fibona,
        file: "fibona.snek",
        input: "3",
        expected: "3",
    },
    {
        name: ack,
        file: "ack.snek",
        expected: "5",
    },
    {
        name: addfun,
        file: "addfun.snek",
        expected: "5",
    },
    {
        name: tuple,
        file: "tuple.snek",
        expected: "20\n50"
    },
    {
        name: point,
        file: "point.snek",
        expected: "[3, 4]\n[1, 2]\n[4, 6]\n[4, 6]",
    },
    {
        name: bst,
        file: "bst.snek",
        expected: "[5, [1, nil, [2, nil, [3, nil, [4, nil, nil]]]], [6, nil, [7, nil, [8, nil, [9, nil, [10, nil, nil]]]]]]"
    }
    
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: overflow_3,
        file: "overflow_3.snek",
        expected: "overflow",
    },
    {
        name: tuple_error_tag,
        file: "tuple_error_tag.snek",
        expected: "index to not array",
    },
    {
        name: tuple_error_bounds,
        file: "tuple_error_bounds.snek",
        expected: "index out of range",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: let_no_bind,
        file: "let_no_bind.snek",
        expected: "Invalid",
    },
    {
        name: improper_let,
        file: "improper_let.snek",
        expected: "Invalid",
    },
    {
        name: improper_let2,
        file: "improper_let2.snek",
        expected: "Invalid",
    },
    {
        name: miss_pare,
        file: "miss_pare.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: error_define,
        file: "error_define.snek",
        expected: "",
    },
    {
        name: tuple_error3,
        file: "tuple_error3.snek",
        expected: "Invalid",
    }

}



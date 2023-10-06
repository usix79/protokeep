import { Record, Union } from "../fable_modules/fable-library.4.1.4/Types.js";
import { enum_type, class_type, record_type, union_type, option_type, string_type, int32_type } from "../fable_modules/fable-library.4.1.4/Reflection.js";
import { FsharpTypes_Key } from "./Protokeep.fs.js";
import { comparePrimitives, Lazy } from "../fable_modules/fable-library.4.1.4/Util.js";
import { minValue } from "../fable_modules/fable-library.4.1.4/Date.js";
import { empty } from "../fable_modules/fable-library.4.1.4/Map.js";

export class Op extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Unknown", "Val", "Sum", "Mul", "Div", "Ln", "Quantum", "Imagine", "Zero"];
    }
}

export function Op_$reflection() {
    return union_type("Domain.Op", [], Op, () => [[], [["p1", int32_type]], [["p1", Op_$reflection()], ["p2", Op_$reflection()]], [["p1", Op_$reflection()], ["p2", Op_$reflection()]], [["p1", Op_$reflection()], ["p2", Op_$reflection()]], [["p1", Op_$reflection()]], [["p1", Op_$reflection()], ["p2", Op_$reflection()], ["p3", string_type]], [["p1", option_type(int32_type)]], []]);
}

export function Op_MakeUnknownKey() {
    return new FsharpTypes_Key(0, ["0"]);
}

export function Op_MakeValKey() {
    return new FsharpTypes_Key(0, ["1"]);
}

export function Op_MakeSumKey() {
    return new FsharpTypes_Key(0, ["2"]);
}

export function Op_MakeMulKey() {
    return new FsharpTypes_Key(0, ["3"]);
}

export function Op_MakeDivKey() {
    return new FsharpTypes_Key(0, ["4"]);
}

export function Op_MakeLnKey() {
    return new FsharpTypes_Key(0, ["5"]);
}

export function Op_MakeQuantumKey() {
    return new FsharpTypes_Key(0, ["6"]);
}

export function Op_MakeImagineKey() {
    return new FsharpTypes_Key(0, ["7"]);
}

export function Op_MakeZeroKey() {
    return new FsharpTypes_Key(0, ["8"]);
}

export function Op__get_Key(x) {
    switch (x.tag) {
        case 1:
            return Op_MakeValKey();
        case 2:
            return Op_MakeSumKey();
        case 3:
            return Op_MakeMulKey();
        case 4:
            return Op_MakeDivKey();
        case 5:
            return Op_MakeLnKey();
        case 6:
            return Op_MakeQuantumKey();
        case 7:
            return Op_MakeImagineKey();
        case 8:
            return Op_MakeZeroKey();
        default:
            return Op_MakeUnknownKey();
    }
}

export class OpError extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Unknown", "General", "DivisionByZero", "NotSupported"];
    }
}

export function OpError_$reflection() {
    return union_type("Domain.OpError", [], OpError, () => [[], [["p1", string_type]], [], []]);
}

export function OpError_MakeUnknownKey() {
    return new FsharpTypes_Key(0, ["0"]);
}

export function OpError_MakeGeneralKey() {
    return new FsharpTypes_Key(0, ["1"]);
}

export function OpError_MakeDivisionByZeroKey() {
    return new FsharpTypes_Key(0, ["2"]);
}

export function OpError_MakeNotSupportedKey() {
    return new FsharpTypes_Key(0, ["3"]);
}

export function OpError__get_Key(x) {
    switch (x.tag) {
        case 1:
            return OpError_MakeGeneralKey();
        case 2:
            return OpError_MakeDivisionByZeroKey();
        case 3:
            return OpError_MakeNotSupportedKey();
        default:
            return OpError_MakeUnknownKey();
    }
}

export class OpResult extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Unknown", "Success", "Fail"];
    }
}

export function OpResult_$reflection() {
    return union_type("Domain.OpResult", [], OpResult, () => [[], [["p1", int32_type]], [["p1", OpError_$reflection()]]]);
}

export function OpResult_MakeUnknownKey() {
    return new FsharpTypes_Key(0, ["0"]);
}

export function OpResult_MakeSuccessKey() {
    return new FsharpTypes_Key(0, ["1"]);
}

export function OpResult_MakeFailKey() {
    return new FsharpTypes_Key(0, ["2"]);
}

export function OpResult__get_Key(x) {
    switch (x.tag) {
        case 1:
            return OpResult_MakeSuccessKey();
        case 2:
            return OpResult_MakeFailKey();
        default:
            return OpResult_MakeUnknownKey();
    }
}

export class Request extends Record {
    constructor(Token, Operation) {
        super();
        this.Token = Token;
        this.Operation = Operation;
    }
}

export function Request_$reflection() {
    return record_type("Domain.Request", [], Request, () => [["Token", string_type], ["Operation", Op_$reflection()]]);
}

export function Request_get_Default() {
    return new Lazy(() => (new Request("", new Op(0, []))));
}

export class Response extends Record {
    constructor(Token, Result, ExecutionTime, Extra, Since, Tags, Status) {
        super();
        this.Token = Token;
        this.Result = Result;
        this.ExecutionTime = ExecutionTime;
        this.Extra = Extra;
        this.Since = Since;
        this.Tags = Tags;
        this.Status = (Status | 0);
    }
}

export function Response_$reflection() {
    return record_type("Domain.Response", [], Response, () => [["Token", string_type], ["Result", OpResult_$reflection()], ["ExecutionTime", class_type("System.TimeSpan")], ["Extra", option_type(string_type)], ["Since", class_type("System.DateTime")], ["Tags", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, string_type])], ["Status", enum_type("Domain.Subdomain.Status", int32_type, [["Unknown", 0], ["Green", 1], ["Yellow", 2], ["Red", 3]])]]);
}

export function Response_get_Default() {
    return new Lazy(() => (new Response("", new OpResult(0, []), 0, void 0, minValue(), empty({
        Compare: comparePrimitives,
    }), 0)));
}


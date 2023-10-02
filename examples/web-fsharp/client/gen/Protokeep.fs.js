import { toText, printf, toFail, substring, join } from "../fable_modules/fable-library.4.1.4/String.js";
import { map } from "../fable_modules/fable-library.4.1.4/List.js";
import { Union, toString } from "../fable_modules/fable-library.4.1.4/Types.js";
import { union_type, list_type, string_type } from "../fable_modules/fable-library.4.1.4/Reflection.js";
import { empty } from "../fable_modules/fable-library.4.1.4/Map.js";
import { comparePrimitives } from "../fable_modules/fable-library.4.1.4/Util.js";
import { toString as toString_1, parse } from "../fable_modules/fable-library.4.1.4/Date.js";
import { match } from "../fable_modules/fable-library.4.1.4/RegExp.js";
import { fromFloat64, toInt64, toFloat64 } from "../fable_modules/fable-library.4.1.4/BigInt.js";
import { parse as parse_1 } from "../fable_modules/fable-library.4.1.4/Long.js";
import { parse as parse_2 } from "../fable_modules/fable-library.4.1.4/Int32.js";
import { milliseconds as milliseconds_1, totalSeconds } from "../fable_modules/fable-library.4.1.4/TimeSpan.js";

export class FsharpTypes_Key extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Value", "Items", "Inner"];
    }
    toString() {
        const x = this;
        return (x.tag === 1) ? join(",", map(toString, x.fields[0])) : ((x.tag === 2) ? (`(${x.fields[0]})`) : x.fields[0]);
    }
}

export function FsharpTypes_Key_$reflection() {
    return union_type("Protokeep.FsharpTypes.Key", [], FsharpTypes_Key, () => [[["Item", string_type]], [["Item", list_type(FsharpTypes_Key_$reflection())]], [["Item", FsharpTypes_Key_$reflection()]]]);
}

export function FsharpTypes_$007CTryFind$007C_$007C(f, key) {
    return f(key);
}

export function FableConverterHelpers_getProps(_arg) {
    if (_arg.tag === 5) {
        return _arg.fields[0];
    }
    else {
        return empty({
            Compare: comparePrimitives,
        });
    }
}

export function FableConverterHelpers_ifBool(action, _arg) {
    if (_arg.tag === 2) {
        action(_arg.fields[0]);
    }
}

export function FableConverterHelpers_ifString(action, _arg) {
    if (_arg.tag === 1) {
        action(_arg.fields[0]);
    }
}

export function FableConverterHelpers_ifNumber(action, _arg) {
    if (_arg.tag === 0) {
        action(_arg.fields[0]);
    }
}

export function FableConverterHelpers_ifObject(action, _arg) {
    if (_arg.tag === 5) {
        action(_arg.fields[0]);
    }
}

export function FableConverterHelpers_ifArray(action, _arg) {
    if (_arg.tag === 4) {
        action(_arg.fields[0]);
    }
}

export function FableConverterHelpers_toDateTime(v) {
    return parse(v);
}

export function FableConverterHelpers_fromDateTime(v) {
    return toString_1(v, "O");
}

export const FableConverterHelpers_durationRegex = /^(-)?([0-9]{1,12})(\.[0-9]{1,9})?s$/gu;

export const FableConverterHelpers_subsecondScalingFactors = new Int32Array([0, 100000000, 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10, 1]);

export function FableConverterHelpers_toTimeSpan(v) {
    const m = match(FableConverterHelpers_durationRegex, v);
    if (m != null) {
        const signText = m[1] || "";
        const secondsText = m[2] || "";
        const subseconds = m[3] || "";
        return ((signText === "-") ? -1 : 1) * ((toFloat64(toInt64(parse_1(secondsText, 511, false, 64))) * 1000) + ((subseconds !== "") ? ~~((parse_2(substring(subseconds, 1), 511, false, 32) * FableConverterHelpers_subsecondScalingFactors[subseconds.length]) / 1000000) : 0));
    }
    else {
        return toFail(printf("Invalid Duration value: %s"))(v);
    }
}

export function FableConverterHelpers_fromTimeSpan(v) {
    const arg = toInt64(fromFloat64(totalSeconds(v)));
    const arg_1 = milliseconds_1(v) | 0;
    return toText(printf("%d.%ds"))(arg)(arg_1);
}


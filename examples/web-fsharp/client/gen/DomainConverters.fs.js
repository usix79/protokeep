import { class_type } from "../fable_modules/fable-library.4.1.4/Reflection.js";
import { Response, Request, OpResult, OpError, Op } from "./Domain.fs.js";
import { append, singleton as singleton_1, empty, delay, toList, iterate } from "../fable_modules/fable-library.4.1.4/Seq.js";
import { FableConverterHelpers_fromDateTime, FableConverterHelpers_fromTimeSpan, FableConverterHelpers_ifObject, FableConverterHelpers_toDateTime, FableConverterHelpers_toTimeSpan, FableConverterHelpers_ifString, FableConverterHelpers_getProps, FableConverterHelpers_ifBool, FableConverterHelpers_ifNumber } from "./Protokeep.fs.js";
import { map, ofSeq, iterate as iterate_1, empty as empty_1, ofList } from "../fable_modules/fable-library.4.1.4/Map.js";
import { Json } from "../fable_modules/Fable.SimpleJson.3.17.0/Json.fs.js";
import { ofArray, singleton } from "../fable_modules/fable-library.4.1.4/List.js";
import { Lazy, comparePrimitives } from "../fable_modules/fable-library.4.1.4/Util.js";
import { minValue } from "../fable_modules/fable-library.4.1.4/Date.js";
import { ConvertDomainSubdomain_get_StatusToString, ConvertDomainSubdomain_get_StatusFromString, ConvertDomainSubdomain_get_DefaultStatus } from "./SubdomainConverters.fs.js";

export class ConvertDomain {
    constructor() {
    }
}

export function ConvertDomain_$reflection() {
    return class_type("Protokeep.FableConverters.ConvertDomain", void 0, ConvertDomain);
}

export function ConvertDomain_$ctor() {
    return new ConvertDomain();
}

export function ConvertDomain_OpFromJson_Z3E28EAD9(json) {
    let y = new Op(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Val": {
                FableConverterHelpers_ifNumber((v) => {
                    y = (new Op(1, [v]));
                }, pair[1]);
                break;
            }
            case "Sum": {
                y = ConvertDomain_OpCaseSumFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Mul": {
                y = ConvertDomain_OpCaseMulFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Div": {
                y = ConvertDomain_OpCaseDivFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Ln": {
                y = (new Op(5, [ConvertDomain_OpFromJson_Z3E28EAD9(pair[1])]));
                break;
            }
            case "Quantum": {
                y = ConvertDomain_OpCaseQuantumFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Imagine": {
                y = ConvertDomain_OpCaseImagineFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Zero": {
                FableConverterHelpers_ifBool((v_7) => {
                    y = (new Op(8, []));
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpToJson_777D6174(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["Val", new Json(0, [x.fields[0]])] : ((x.tag === 2) ? ["Sum", ConvertDomain_OpCaseSumToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 3) ? ["Mul", ConvertDomain_OpCaseMulToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 4) ? ["Div", ConvertDomain_OpCaseDivToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 5) ? ["Ln", ConvertDomain_OpToJson_777D6174(x.fields[0])] : ((x.tag === 6) ? ["Quantum", ConvertDomain_OpCaseQuantumToJson_Z68EE3D45(x.fields[0], x.fields[1], x.fields[2])] : ((x.tag === 7) ? ["Imagine", ConvertDomain_OpCaseImagineToJson_71136F3F(x.fields[0])] : ((x.tag === 8) ? ["Zero", new Json(2, [true])] : ["Unknown", new Json(2, [true])])))))))), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpCaseSumFromJson_Z3E28EAD9(json) {
    let p1 = new Op(0, []);
    let p2 = new Op(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "P1": {
                p1 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "P2": {
                p2 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Op(2, [p1, p2]);
}

export function ConvertDomain_OpCaseSumToJson_1054EE80(p1, p2) {
    return new Json(5, [ofList(ofArray([["P1", ConvertDomain_OpToJson_777D6174(p1)], ["P2", ConvertDomain_OpToJson_777D6174(p2)]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpCaseMulFromJson_Z3E28EAD9(json) {
    let p1 = new Op(0, []);
    let p2 = new Op(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "P1": {
                p1 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "P2": {
                p2 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Op(3, [p1, p2]);
}

export function ConvertDomain_OpCaseMulToJson_1054EE80(p1, p2) {
    return new Json(5, [ofList(ofArray([["P1", ConvertDomain_OpToJson_777D6174(p1)], ["P2", ConvertDomain_OpToJson_777D6174(p2)]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpCaseDivFromJson_Z3E28EAD9(json) {
    let p1 = new Op(0, []);
    let p2 = new Op(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "P1": {
                p1 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "P2": {
                p2 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Op(4, [p1, p2]);
}

export function ConvertDomain_OpCaseDivToJson_1054EE80(p1, p2) {
    return new Json(5, [ofList(ofArray([["P1", ConvertDomain_OpToJson_777D6174(p1)], ["P2", ConvertDomain_OpToJson_777D6174(p2)]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpCaseQuantumFromJson_Z3E28EAD9(json) {
    let p1 = new Op(0, []);
    let p2 = new Op(0, []);
    let p3 = "";
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "P1": {
                p1 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "P2": {
                p2 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "P3": {
                FableConverterHelpers_ifString((v_2) => {
                    p3 = v_2;
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Op(6, [p1, p2, p3]);
}

export function ConvertDomain_OpCaseQuantumToJson_Z68EE3D45(p1, p2, p3) {
    return new Json(5, [ofList(ofArray([["P1", ConvertDomain_OpToJson_777D6174(p1)], ["P2", ConvertDomain_OpToJson_777D6174(p2)], ["P3", new Json(1, [p3])]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpCaseImagineFromJson_Z3E28EAD9(json) {
    let p1 = void 0;
    iterate((pair) => {
        if (pair[0] === "P1Value") {
            FableConverterHelpers_ifNumber((v) => {
                p1 = v;
            }, pair[1]);
        }
    }, FableConverterHelpers_getProps(json));
    return new Op(7, [p1]);
}

export function ConvertDomain_OpCaseImagineToJson_71136F3F(p1) {
    return new Json(5, [ofList(toList(delay(() => {
        if (p1 == null) {
            return empty();
        }
        else {
            return singleton_1(["P1Value", new Json(0, [p1])]);
        }
    })), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpErrorFromJson_Z3E28EAD9(json) {
    let y = new OpError(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "General": {
                FableConverterHelpers_ifString((v) => {
                    y = (new OpError(1, [v]));
                }, pair[1]);
                break;
            }
            case "DivisionByZero": {
                FableConverterHelpers_ifBool((v_1) => {
                    y = (new OpError(2, []));
                }, pair[1]);
                break;
            }
            case "NotSupported": {
                FableConverterHelpers_ifBool((v_2) => {
                    y = (new OpError(3, []));
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpErrorToJson_Z43B5734(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["General", new Json(1, [x.fields[0]])] : ((x.tag === 2) ? ["DivisionByZero", new Json(2, [true])] : ((x.tag === 3) ? ["NotSupported", new Json(2, [true])] : ["Unknown", new Json(2, [true])]))), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpResultFromJson_Z3E28EAD9(json) {
    let y = new OpResult(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Success": {
                FableConverterHelpers_ifNumber((v) => {
                    y = (new OpResult(1, [v]));
                }, pair[1]);
                break;
            }
            case "Fail": {
                y = (new OpResult(2, [ConvertDomain_OpErrorFromJson_Z3E28EAD9(pair[1])]));
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpResultToJson_5687C0FD(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["Success", new Json(0, [x.fields[0]])] : ((x.tag === 2) ? ["Fail", ConvertDomain_OpErrorToJson_Z43B5734(x.fields[0])] : ["Unknown", new Json(2, [true])])), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_get_DefaultRequest() {
    return new Lazy(() => (new Request("", new Op(0, []))));
}

export function ConvertDomain_RequestFromJson_Z3E28EAD9(json) {
    let vToken = "";
    let vOperation = new Op(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Token": {
                FableConverterHelpers_ifString((v) => {
                    vToken = v;
                }, pair[1]);
                break;
            }
            case "Operation": {
                vOperation = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Request(vToken, vOperation);
}

export function ConvertDomain_RequestToJson_Z65DB4746(x) {
    return new Json(5, [ofList(ofArray([["Token", new Json(1, [x.Token])], ["Operation", ConvertDomain_OpToJson_777D6174(x.Operation)]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_get_DefaultResponse() {
    return new Lazy(() => (new Response("", new OpResult(0, []), 0, void 0, minValue(), empty_1({
        Compare: comparePrimitives,
    }), ConvertDomainSubdomain_get_DefaultStatus().Value)));
}

export function ConvertDomain_ResponseFromJson_Z3E28EAD9(json) {
    let vToken = "";
    let vResult = new OpResult(0, []);
    let vExecutionTime = 0;
    let vExtra = void 0;
    let vSince = minValue();
    let vTags = [];
    let vStatus = ConvertDomainSubdomain_get_DefaultStatus().Value;
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Token": {
                FableConverterHelpers_ifString((v) => {
                    vToken = v;
                }, pair[1]);
                break;
            }
            case "Result": {
                vResult = ConvertDomain_OpResultFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "ExecutionTime": {
                FableConverterHelpers_ifString((v_2) => {
                    vExecutionTime = FableConverterHelpers_toTimeSpan(v_2);
                }, pair[1]);
                break;
            }
            case "ExtraValue": {
                FableConverterHelpers_ifString((v_4) => {
                    vExtra = v_4;
                }, pair[1]);
                break;
            }
            case "Since": {
                FableConverterHelpers_ifString((v_5) => {
                    vSince = FableConverterHelpers_toDateTime(v_5);
                }, pair[1]);
                break;
            }
            case "Tags": {
                FableConverterHelpers_ifObject((table) => {
                    iterate_1((key, _arg_4) => {
                        FableConverterHelpers_ifString((v_7) => {
                            void (vTags.push([key, v_7]));
                        }, _arg_4);
                    }, table);
                }, pair[1]);
                break;
            }
            case "Status": {
                FableConverterHelpers_ifString((v_9) => {
                    vStatus = (ConvertDomainSubdomain_get_StatusFromString()(v_9) | 0);
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FableConverterHelpers_getProps(json));
    return new Response(vToken, vResult, vExecutionTime, vExtra, vSince, ofSeq(vTags, {
        Compare: comparePrimitives,
    }), vStatus);
}

export function ConvertDomain_ResponseToJson_Z135E8A98(x) {
    return new Json(5, [ofList(toList(delay(() => append(singleton_1(["Token", new Json(1, [x.Token])]), delay(() => append(singleton_1(["Result", ConvertDomain_OpResultToJson_5687C0FD(x.Result)]), delay(() => append(singleton_1(["ExecutionTime", new Json(1, [FableConverterHelpers_fromTimeSpan(x.ExecutionTime)])]), delay(() => {
        let matchValue;
        return append((matchValue = x.Extra, (matchValue == null) ? (empty()) : singleton_1(["ExtraValue", new Json(1, [matchValue])])), delay(() => append(singleton_1(["Since", new Json(1, [FableConverterHelpers_fromDateTime(x.Since)])]), delay(() => append(singleton_1(["Tags", new Json(5, [map((_arg, v_3) => (new Json(1, [v_3])), x.Tags)])]), delay(() => singleton_1(["Status", new Json(1, [ConvertDomainSubdomain_get_StatusToString()(x.Status)])])))))));
    })))))))), {
        Compare: comparePrimitives,
    })]);
}


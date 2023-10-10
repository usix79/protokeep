import { class_type } from "./fable_modules/fable-library.4.1.4/Reflection.js";
import { Response, Request, OpResult, OpError, Op } from "./Domain.g.fs.js";
import { append, singleton as singleton_1, empty, delay, toList, map as map_1, iterate } from "./fable_modules/fable-library.4.1.4/Seq.js";
import { FsharpFableHelpers_fromDateTime, FsharpFableHelpers_fromTimeSpan, FsharpFableHelpers_toDateTime, FsharpFableHelpers_toTimeSpan, FsharpFableHelpers_mapToArray, FsharpFableHelpers_ifMap, FsharpFableHelpers_ifString, FsharpFableHelpers_getProps, FsharpFableHelpers_ifBool, FsharpFableHelpers_ifArray, FsharpFableHelpers_ifNumber } from "./Protokeep.g.fs.js";
import { ofArray, singleton, ofSeq } from "./fable_modules/fable-library.4.1.4/List.js";
import { ofSeq as ofSeq_1, ofList } from "./fable_modules/fable-library.4.1.4/Map.js";
import { Json } from "./fable_modules/Fable.SimpleJson.3.17.0/Json.fs.js";
import { comparePrimitives } from "./fable_modules/fable-library.4.1.4/Util.js";
import { minValue } from "./fable_modules/fable-library.4.1.4/Date.js";
import { ConvertDomainSubdomain_get_StatusToString, ConvertDomainSubdomain_get_StatusFromString } from "./SubdomainJson.g.fs.js";

export class ConvertDomain {
    constructor() {
    }
}

export function ConvertDomain_$reflection() {
    return class_type("Domain.JsonConverters.ConvertDomain", void 0, ConvertDomain);
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
                let _p1 = 0;
                FsharpFableHelpers_ifNumber((v) => {
                    _p1 = (v | 0);
                }, pair[1]);
                y = (new Op(1, [_p1]));
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
                let _p1_1 = new Op(0, []);
                _p1_1 = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                y = (new Op(5, [_p1_1]));
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
            case "SumAll": {
                let _p1_2 = [];
                FsharpFableHelpers_ifArray((source) => {
                    iterate((v_7) => {
                        const arg_9 = ConvertDomain_OpFromJson_Z3E28EAD9(v_7);
                        void (_p1_2.push(arg_9));
                    }, source);
                }, pair[1]);
                y = (new Op(8, [ofSeq(_p1_2)]));
                break;
            }
            case "Zero": {
                FsharpFableHelpers_ifBool((v_8) => {
                    y = (new Op(9, []));
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpToJson_777D6174(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["Val", new Json(0, [x.fields[0]])] : ((x.tag === 2) ? ["Sum", ConvertDomain_OpCaseSumToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 3) ? ["Mul", ConvertDomain_OpCaseMulToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 4) ? ["Div", ConvertDomain_OpCaseDivToJson_1054EE80(x.fields[0], x.fields[1])] : ((x.tag === 5) ? ["Ln", ConvertDomain_OpToJson_777D6174(x.fields[0])] : ((x.tag === 6) ? ["Quantum", ConvertDomain_OpCaseQuantumToJson_Z68EE3D45(x.fields[0], x.fields[1], x.fields[2])] : ((x.tag === 7) ? ["Imagine", ConvertDomain_OpCaseImagineToJson_Z28A5E458(x.fields[0])] : ((x.tag === 8) ? ["SumAll", new Json(4, [ofSeq(map_1(ConvertDomain_OpToJson_777D6174, x.fields[0]))])] : ((x.tag === 9) ? ["Zero", new Json(2, [true])] : ["Unknown", new Json(2, [true])]))))))))), {
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
    }, FsharpFableHelpers_getProps(json));
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
    }, FsharpFableHelpers_getProps(json));
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
    }, FsharpFableHelpers_getProps(json));
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
                FsharpFableHelpers_ifString((v_2) => {
                    p3 = v_2;
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
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
            FsharpFableHelpers_ifNumber((v) => {
                p1 = v;
            }, pair[1]);
        }
    }, FsharpFableHelpers_getProps(json));
    return new Op(7, [p1]);
}

export function ConvertDomain_OpCaseImagineToJson_Z28A5E458(p1) {
    return new Json(5, [ofList(toList(delay(() => {
        if (p1 == null) {
            return empty();
        }
        else {
            return singleton_1(["{firstCharToUpper fieldInfo.Name}Value", new Json(0, [p1])]);
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
                let _p1 = [];
                FsharpFableHelpers_ifArray((source) => {
                    iterate((_arg) => {
                        FsharpFableHelpers_ifString((v) => {
                            void (_p1.push(v));
                        }, _arg);
                    }, source);
                }, pair[1]);
                y = (new OpError(1, [ofSeq(_p1)]));
                break;
            }
            case "DivisionByZero": {
                FsharpFableHelpers_ifBool((v_1) => {
                    y = (new OpError(2, []));
                }, pair[1]);
                break;
            }
            case "NotSupported": {
                FsharpFableHelpers_ifBool((v_2) => {
                    y = (new OpError(3, []));
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpErrorToJson_Z43B5734(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["General", new Json(4, [ofSeq(map_1((v) => (new Json(1, [v])), x.fields[0]))])] : ((x.tag === 2) ? ["DivisionByZero", new Json(2, [true])] : ((x.tag === 3) ? ["NotSupported", new Json(2, [true])] : ["Unknown", new Json(2, [true])]))), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_OpResultFromJson_Z3E28EAD9(json) {
    let y = new OpResult(0, []);
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Success": {
                let _p1 = 0;
                FsharpFableHelpers_ifNumber((v) => {
                    _p1 = (v | 0);
                }, pair[1]);
                y = (new OpResult(1, [_p1]));
                break;
            }
            case "Fail": {
                let _p1_1 = new OpError(0, []);
                _p1_1 = ConvertDomain_OpErrorFromJson_Z3E28EAD9(pair[1]);
                y = (new OpResult(2, [_p1_1]));
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
    return y;
}

export function ConvertDomain_OpResultToJson_5687C0FD(x) {
    return new Json(5, [ofList(singleton((x.tag === 1) ? ["Success", new Json(0, [x.fields[0]])] : ((x.tag === 2) ? ["Fail", ConvertDomain_OpErrorToJson_Z43B5734(x.fields[0])] : ["Unknown", new Json(2, [true])])), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_RequestFromJson_Z3E28EAD9(json) {
    let vToken = "";
    let vOperation = new Op(0, []);
    let vTags = [];
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Token": {
                FsharpFableHelpers_ifString((v) => {
                    vToken = v;
                }, pair[1]);
                break;
            }
            case "Operation": {
                vOperation = ConvertDomain_OpFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "Tags": {
                FsharpFableHelpers_ifMap((tupledArg) => {
                    FsharpFableHelpers_ifString((v_2) => {
                        FsharpFableHelpers_ifString((v_3) => {
                            void (vTags.push([v_2, v_3]));
                        }, tupledArg[1]);
                    }, tupledArg[0]);
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
    return new Request(vToken, vOperation, ofSeq_1(vTags, {
        Compare: comparePrimitives,
    }));
}

export function ConvertDomain_RequestToJson_Z65DB4746(x) {
    return new Json(5, [ofList(ofArray([["Token", new Json(1, [x.Token])], ["Operation", ConvertDomain_OpToJson_777D6174(x.Operation)], ["Tags", FsharpFableHelpers_mapToArray((k) => (new Json(1, [k])), (v) => (new Json(1, [v])), x.Tags)]]), {
        Compare: comparePrimitives,
    })]);
}

export function ConvertDomain_ResponseFromJson_Z3E28EAD9(json) {
    let vToken = "";
    let vResult = new OpResult(0, []);
    let vExecutionTime = 0;
    let vExtra = void 0;
    let vSince = minValue();
    let vTags = [];
    let vStatus = 0;
    iterate((pair) => {
        const matchValue = pair[0];
        switch (matchValue) {
            case "Token": {
                FsharpFableHelpers_ifString((v) => {
                    vToken = v;
                }, pair[1]);
                break;
            }
            case "Result": {
                vResult = ConvertDomain_OpResultFromJson_Z3E28EAD9(pair[1]);
                break;
            }
            case "ExecutionTime": {
                FsharpFableHelpers_ifString((v_2) => {
                    vExecutionTime = FsharpFableHelpers_toTimeSpan(v_2);
                }, pair[1]);
                break;
            }
            case "ExtraValue": {
                FsharpFableHelpers_ifString((v_4) => {
                    vExtra = v_4;
                }, pair[1]);
                break;
            }
            case "Since": {
                FsharpFableHelpers_ifString((v_5) => {
                    vSince = FsharpFableHelpers_toDateTime(v_5);
                }, pair[1]);
                break;
            }
            case "Tags": {
                FsharpFableHelpers_ifMap((tupledArg) => {
                    FsharpFableHelpers_ifString((v_7) => {
                        FsharpFableHelpers_ifString((v_8) => {
                            void (vTags.push([v_7, v_8]));
                        }, tupledArg[1]);
                    }, tupledArg[0]);
                }, pair[1]);
                break;
            }
            case "Status": {
                FsharpFableHelpers_ifString((v_10) => {
                    vStatus = (ConvertDomainSubdomain_get_StatusFromString()(v_10) | 0);
                }, pair[1]);
                break;
            }
            default:
                0;
        }
    }, FsharpFableHelpers_getProps(json));
    return new Response(vToken, vResult, vExecutionTime, vExtra, vSince, ofSeq_1(vTags, {
        Compare: comparePrimitives,
    }), vStatus);
}

export function ConvertDomain_ResponseToJson_Z135E8A98(x) {
    return new Json(5, [ofList(toList(delay(() => append(singleton_1(["Token", new Json(1, [x.Token])]), delay(() => append(singleton_1(["Result", ConvertDomain_OpResultToJson_5687C0FD(x.Result)]), delay(() => append(singleton_1(["ExecutionTime", new Json(1, [FsharpFableHelpers_fromTimeSpan(x.ExecutionTime)])]), delay(() => {
        let matchValue;
        return append((matchValue = x.Extra, (matchValue == null) ? (empty()) : singleton_1(["{firstCharToUpper fieldInfo.Name}Value", new Json(1, [matchValue])])), delay(() => append(singleton_1(["Since", new Json(1, [FsharpFableHelpers_fromDateTime(x.Since)])]), delay(() => append(singleton_1(["Tags", FsharpFableHelpers_mapToArray((k) => (new Json(1, [k])), (v_3) => (new Json(1, [v_3])), x.Tags)]), delay(() => singleton_1(["Status", new Json(1, [ConvertDomainSubdomain_get_StatusToString()(x.Status)])])))))));
    })))))))), {
        Compare: comparePrimitives,
    })]);
}


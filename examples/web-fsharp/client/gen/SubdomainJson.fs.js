import { class_type } from "../fable_modules/fable-library.4.1.4/Reflection.js";

export class ConvertDomainSubdomain {
    constructor() {
    }
}

export function ConvertDomainSubdomain_$reflection() {
    return class_type("Domain.JsonConverters.ConvertDomainSubdomain", void 0, ConvertDomainSubdomain);
}

export function ConvertDomainSubdomain_$ctor() {
    return new ConvertDomainSubdomain();
}

export function ConvertDomainSubdomain_get_StatusFromString() {
    return (_arg) => ((_arg === "StatusGreen") ? 1 : ((_arg === "StatusYellow") ? 2 : ((_arg === "StatusRed") ? 3 : 0)));
}

export function ConvertDomainSubdomain_get_StatusToString() {
    return (_arg) => ((_arg === 1) ? "StatusGreen" : ((_arg === 2) ? "StatusYellow" : ((_arg === 3) ? "StatusRed" : "Unknown")));
}


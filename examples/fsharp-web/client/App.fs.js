import { PromiseBuilder__Delay_62FBFDE1, PromiseBuilder__Run_212F1D4B } from "./fable_modules/Fable.Promise.2.0.0/Promise.fs.js";
import { Request, Op } from "./Domain.g.fs.js";
import { ofArray } from "./fable_modules/fable-library.4.1.4/List.js";
import { ofList } from "./fable_modules/fable-library.4.1.4/Map.js";
import { comparePrimitives } from "./fable_modules/fable-library.4.1.4/Util.js";
import { printf, toText } from "./fable_modules/fable-library.4.1.4/String.js";
import { SimpleJson_parse, SimpleJson_toString } from "./fable_modules/Fable.SimpleJson.3.17.0/SimpleJson.fs.js";
import { ConvertDomain_ResponseFromJson_Z3E28EAD9, ConvertDomain_RequestToJson_Z65DB4746 } from "./DomainJson.g.fs.js";
import { fetch$, Types_RequestProperties } from "./fable_modules/Fable.Fetch.2.2.0/Fetch.fs.js";
import { promise } from "./fable_modules/Fable.Promise.2.0.0/PromiseImpl.fs.js";

(function () {
    const pr = PromiseBuilder__Run_212F1D4B(promise, PromiseBuilder__Delay_62FBFDE1(promise, () => {
        const req = new Request("WebClient-123", new Op(2, [new Op(8, [ofArray([new Op(1, [2]), new Op(1, [3]), new Op(1, [4])])]), new Op(2, [new Op(7, [40]), new Op(3, [new Op(1, [100500]), new Op(9, [])])])]), ofList(ofArray([["origin", "test"], ["cookie", "123"]]), {
            Compare: comparePrimitives,
        }));
        document.getElementById("reqObj").innerText = toText(printf("%A"))(req);
        const jsonTxt = SimpleJson_toString(ConvertDomain_RequestToJson_Z65DB4746(req));
        document.getElementById("reqJson").innerText = jsonTxt;
        const props = ofArray([new Types_RequestProperties(0, ["POST"]), new Types_RequestProperties(1, [{
            "Content-Type": "application/json",
        }]), new Types_RequestProperties(2, [jsonTxt])]);
        return fetch$("https://localhost:5001/calc", props).then((_arg) => (_arg.text().then((_arg_1) => {
            const respJsonTxt = _arg_1;
            document.getElementById("respJson").innerText = respJsonTxt;
            const resp_1 = ConvertDomain_ResponseFromJson_Z3E28EAD9(SimpleJson_parse(respJsonTxt));
            document.getElementById("respObj").innerText = toText(printf("%A"))(resp_1);
            return Promise.resolve();
        })));
    }));
    pr.then();
})();


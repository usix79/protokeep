import { PromiseBuilder__Delay_62FBFDE1, PromiseBuilder__Run_212F1D4B } from "./.fable/Fable.Promise.2.0.0/Promise.fs.js";
import { fetch$, Types_RequestProperties } from "./.fable/Fable.Fetch.2.2.0/Fetch.fs.js";
import { ofArray } from "./.fable/fable-library.3.0.1/List.js";
import { promise } from "./.fable/Fable.Promise.2.0.0/PromiseImpl.fs.js";

(function () {
    const pr = PromiseBuilder__Run_212F1D4B(promise, PromiseBuilder__Delay_62FBFDE1(promise, () => {
        const props = ofArray([new Types_RequestProperties(0, "POST"), new Types_RequestProperties(1, {
            ["Content-Type"]: "application/json",
        }), new Types_RequestProperties(2, "{ \"Token\": \"Sigma-7\", \"OperationSum\": { \"P1Val\": { \"P1\": 2 }, \"P2Mul\": { \"P1Val\": { \"P1\": 4 }, \"P2Val\": { \"P1\": 10 } } } }")]);
        return fetch$("https://localhost:5001/calc/json", props).then(((_arg1) => {
            const resp = _arg1;
            return resp.text().then(((_arg2) => {
                const msg = _arg2;
                document.getElementById("header").innerText = msg;
                return Promise.resolve();
            }));
        }));
    }));
    pr.then();
})();


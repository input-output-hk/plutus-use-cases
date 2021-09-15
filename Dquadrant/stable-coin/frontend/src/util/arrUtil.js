import {toAda} from "./stringUtils";

export function getLastItem(arr){
    return arr[arr.length - 1];
}

export function hasData(val){
    return val !== undefined && val !== null && val.length > 0;
}

/* ============================================= */
/* Dashboard utils */
/* ============================================= */
export function getTotalTokens(states, type,  hasCurrencySymbol = true){
    const latestState = getLastItem(states)
    if(latestState !== undefined){
        const token = latestState["getValue"]?.filter(item => {
            const currencySymbol = item[0]?.["unCurrencySymbol"] ?? ""
            const firstTokenName = item[1]?.[0]?.[0]?.["unTokenName"] ?? ""
            const secondTokenName = item[1]?.[1]?.[0]?.["unTokenName"] ?? ""
            return hasCurrencySymbol ? hasData(currencySymbol) && (hasData(firstTokenName) || hasData(secondTokenName)): !hasData(currencySymbol)
        });
        if(hasCurrencySymbol && token?.[0] !== undefined){
            const valToken = token[0][1]?.filter(item => item[0]["unTokenName"] === type)
            return valToken[0] !== undefined ? valToken[0][1]: 0
        }
            return  token?.[0]?.[1]?.[0]?.[1] ?? 0
    }
    return 0;
}

export function getCurrentStateAndRates(states, type){
    const rates = states?.filter(item => item[type] !== undefined)
    const latestRate = rates[rates.length - 1]
    if(type === "currentCoinsRates"){
        return {
            "pegRate": latestRate?.[type]?.["pegRate"] ?? 0,
            "scRate": latestRate?.[type]?.["scRate"] ?? 0,
            "rcRate": latestRate?.[type]?.["rcRate"] ?? 0
        }
    }else {
        return {
            "baseReserveAmount": toAda(latestRate?.[type]?.["baseReserveAmount"]) ?? 0,
            "stableCoinAmount": latestRate?.[type]?.["stableCoinAmount"] ?? 0,
            "reserveCoinAmount": latestRate?.[type]?.["reserveCoinAmount"] ?? 0,
            "policyScript": latestRate?.[type]?.["policyScript"] ?? "",
            "bankFee": latestRate?.[type]?.["bankFee"] ?? "",
            "contractStatus": latestRate?.[type]?.["contractStatus"] ?? ""
        }
    }
}
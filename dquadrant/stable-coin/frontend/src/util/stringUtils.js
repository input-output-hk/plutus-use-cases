// -------------------------------- //
//  Unit conversion  //
//--------------------------------- //

export const shortenStr = (str, expectedLength, firstIndex = 0) => {
    if (expectedLength !== undefined) {
        return str.toString().substr(firstIndex, expectedLength);
    }
    return str;
};

export const getTruncVal = (val, decimalCount = 2) => {
    const valStr = val.toString();
    const intVal = valStr.split(".")[0];
    const decimalVal = valStr.split(".")[1]?.substr(0, decimalCount);
    return decimalVal !==undefined ? `${intVal}.${decimalVal}` : intVal;
};

export const toAda = (lovelaceUnit) => {
    const adaValue= lovelaceUnit * Math.pow(10, -6);
    return +(Math.round(adaValue + "e+6") + "e-6")

};

export const toLovelace = (adaUnit) => {
    return adaUnit * Math.pow(10, 6);
};

export const toLovelaceIfExists = (adaUnit) => {
    return adaUnit !== undefined ? toLovelace(adaUnit) : adaUnit;
};

/* ============================================= */
/* generic  */
/* ============================================= */

export const toPercent = (val, totalVal) => {
    if (totalVal === 0) return 0;
    return (val / totalVal) * Math.pow(10, 2);
};

// -------------------------------- //
//  Positional conversion  //
//--------------------------------- //
export const thousandValue = (value) => {
    return value * Math.pow(10, -3);
};

export const millionValue = (value) => {
    return value * Math.pow(10, -6);
};

export const billionValue = (value) => {
    return value * Math.pow(10, -9);
};

export const trillionValue = (value) => {
    return value * Math.pow(10, -12);
};

// -------------------------------- //
//  Filters  //
//--------------------------------- //

export const isInHundredRange = (value) => {
    return value >= 0 && value < Math.pow(10, 3);
};

export const isInThousandRange = (value) => {
    return value >= Math.pow(10, 3) && value < Math.pow(10, 6);
};

export const isInMillionRange = (value) => {
    return value >= Math.pow(10, 6) && value < Math.pow(10, 9);
};

export const isInBillionRange = (value) => {
    return value >= Math.pow(10, 9) && value < Math.pow(10, 12);
};

export const isInTrillionRange = (value) => {
    return value >= Math.pow(10, 12) && value < Math.pow(10, 15);
};

export const toAggregateStr = (val) => {
    if (isInTrillionRange(val)) {
        return `${getTruncVal(trillionValue(val))}T`;
    }
    if (isInBillionRange(val)) {
        return `${getTruncVal(billionValue(val))}B`;
    }
    if (isInMillionRange(val)) {
        return `${getTruncVal(millionValue(val))}M`;
    }
    if (isInThousandRange(val)) {
        return `${getTruncVal(thousandValue(val))}K`;
    }
    return val;
};

export const toAdaStr = (lovelaceOrAda, expected_length = 6, isLovelace = true) => {
    const ada = isLovelace ? toAda(lovelaceOrAda) : lovelaceOrAda;
    if (isInMillionRange(ada)) {
        return `${shortenStr(millionValue(ada), expected_length)}M`;
    }
    if (isInThousandRange(ada)) {
        return `${shortenStr(thousandValue(ada), expected_length)}K`;
    }
    return shortenStr(ada, expected_length);
};


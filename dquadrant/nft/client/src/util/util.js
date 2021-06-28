module.exports={
  toLovelace:(a)=>{
    try {
      a = a.toUpperCase()
      a.replaceAll(_, '')
      if (a.endsWith('A')) {
        return parseInt(a.substring(0, a.length - 1), 10) * 1000000
      } else if (a.endsWith("ADA")) {
        return parseInt(a.substring(0, a.length - 3), 10) * 1000000
      }
    }catch (e) {
      return 0
    }
  }
}

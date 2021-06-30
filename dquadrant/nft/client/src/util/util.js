module.exports={
  parseTimeStampToDate(timeStamp) {
    const date = new Date(timeStamp * 1000)
    return date.toLocaleString()
  },
  parseDateToTimeStamp(date) {
    return date
  }
}

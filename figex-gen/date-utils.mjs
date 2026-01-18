function formatDate(date, format) {
  const map = {
    'yyyy': date.getFullYear(),
    'yy': (date.getFullYear() + '').slice(-2),
    'MM': (date.getMonth() + 1).toString().padStart(2, '0'),
    'dd': date.getDate().toString().padStart(2, '0'),
    'HH': date.getHours().toString().padStart(2, '0'),
    'mm': date.getMinutes().toString().padStart(2, '0'),
    'ss': date.getSeconds().toString().padStart(2, '0')
  };
  return format.replace(/([a-zA-Z]{4})|([a-zA-Z]{2})/g, match => map[match]);
}

function parseDate(str, format) {
  const tokens = format.match(/([a-zA-Z]{4})|([a-zA-Z]{2})/g) || [];
  const values = {};
  const date = new Date();

  tokens.forEach(token => {
    const value = str.match(new RegExp(`(${token})`, 'g'))[0];
    values[token] = value;
  });

  if (values['yyyy']) date.setFullYear(parseInt(values['yyyy']));
  if (values['MM']) date.setMonth(parseInt(values['MM']) - 1);
  if (values['dd']) date.setDate(parseInt(values['dd']));
  if (values['HH']) date.setHours(parseInt(values['HH']));
  if (values['mm']) date.setMinutes(parseInt(values['mm']));
  if (values['ss']) date.setSeconds(parseInt(values['ss']));

  return date;
}
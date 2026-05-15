const fs = require('fs');
let content = fs.readFileSync('./gen_report.js', 'utf8');

// Escape apostrophes inside Ukrainian/Latin words (they are content, not string delimiters)
// Pattern: letter ' letter  =>  letter \' letter
const cyrillic = 'Ѐ-ӿ';
const re = new RegExp("([a-zA-Z" + cyrillic + "])'([a-zA-Z" + cyrillic + "])", 'g');
const before = (content.match(re) || []).length;
content = content.replace(re, function(m, a, b) { return a + "\\'" + b; });
const after = (content.match(re) || []).length;

fs.writeFileSync('./gen_report.js', content, 'utf8');
console.log('Escaped ' + before + ' inner apostrophes; remaining matches: ' + after);

import * as preprocess from './preprocess';

global.minifyCSS         = preprocess.minifyCSS;
global.scopeComponentCSS = preprocess.scopeComponentCSS;
global.autoprefixCSS     = preprocess.autoprefixCSS;
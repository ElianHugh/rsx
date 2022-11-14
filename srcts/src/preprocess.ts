import * as postcss from 'postcss';
import * as autoprefixer from 'autoprefixer';
import prefixer = require('postcss-prefix-selector');

export function scopeComponentCSS(styleString: string, hash: string): string {
    const prefixerOpts = {
        prefix: `[data-rsx-${hash.replace(/:/g, '\:')}=""]`,
        transform: function (prefix: string, selector: string) {
            return selector + prefix;
        }
    };
    const processor = new postcss.Processor;
    return processor.use(prefixer(prefixerOpts) as postcss.AcceptedPlugin).process(styleString).css;
}

export function autoprefixCSS(styleString: string): string {
    // const processor = new postcss.Processor;
    // const opts: postcss.AcceptedPlugin = autoprefixer.default;
    // return processor.use(opts).process(styleString).css;
    return styleString;
}

export function minifyCSS(styleString: string): string {
    // TODO
    return styleString;
}
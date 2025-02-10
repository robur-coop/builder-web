let static_css = Tyxml.Html.Unsafe.data {|
/*! tailwindcss v3.4.4 | MIT License | https://tailwindcss.com*/

*,
:after,
:before {
    box-sizing: border-box;
    border: 0 solid #e5e7eb
}

:after,
:before {
    --tw-content: ""
}

:host,
html {
    line-height: 1.5;
    -webkit-text-size-adjust: 100%;
    -moz-tab-size: 4;
    -o-tab-size: 4;
    tab-size: 4;
    font-family: ui-sans-serif, system-ui, sans-serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol, Noto Color Emoji;
    font-feature-settings: normal;
    font-variation-settings: normal;
    -webkit-tap-highlight-color: transparent
}

.dark\:bg-black-molly {
    &:where(.dark, .dark *) {
        --tw-bg-opacity: 1;
        background-color: #0B0D12 !important;
        color: #FFFFFF !important
    }
}
.dark\:text-gray-50 {
    &:where(.dark, .dark *) {
        color: #FFFFFF !important
    }
}

nav ul {
  display: flex;
  list-style: none;
}
nav ul li::before {
  content: "→";
}
nav ul li:first-child::before {
  content: "";
}
nav a {
  padding: .5em 1em;
}

body {
    margin: 0;
    line-height: inherit
}

hr {
    height: 0;
    color: inherit;
    border-top-width: 1px
}

abbr:where([title]) {
    -webkit-text-decoration: underline dotted;
    text-decoration: underline dotted
}

h1,
h2,
h3,
h4,
h5,
h6 {
    font-size: inherit;
    font-weight: inherit
}

a {
    color: inherit;
    text-decoration: inherit
}

b,
strong {
    font-weight: bolder
}

code,
kbd,
pre,
samp {
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace;
    font-feature-settings: normal;
    font-variation-settings: normal;
    font-size: 1em
}

small {
    font-size: 80%
}

sub,
sup {
    font-size: 75%;
    line-height: 0;
    position: relative;
    vertical-align: initial
}

sub {
    bottom: -.25em
}

sup {
    top: -.5em
}

table {
    text-indent: 0;
    border-color: inherit;
    border-collapse: collapse
}

button,
input,
optgroup,
select,
textarea {
    font-family: inherit;
    font-feature-settings: inherit;
    font-variation-settings: inherit;
    font-size: 100%;
    font-weight: inherit;
    line-height: inherit;
    letter-spacing: inherit;
    color: inherit;
    margin: 0;
    padding: 0
}

button,
select {
    text-transform: none
}

button,
input:where([type=button]),
input:where([type=reset]),
input:where([type=submit]) {
    -webkit-appearance: button;
    background-color: initial;
    background-image: none
}

:-moz-focusring {
    outline: auto
}

:-moz-ui-invalid {
    box-shadow: none
}

progress {
    vertical-align: initial
}

::-webkit-inner-spin-button,
::-webkit-outer-spin-button {
    height: auto
}

[type=search] {
    -webkit-appearance: textfield;
    outline-offset: -2px
}

::-webkit-search-decoration {
    -webkit-appearance: none
}

::-webkit-file-upload-button {
    -webkit-appearance: button;
    font: inherit
}

summary {
    display: list-item
}

blockquote,
dd,
dl,
figure,
h1,
h2,
h3,
h4,
h5,
h6,
hr,
p,
pre {
    margin: 0
}

fieldset {
    margin: 0
}

fieldset,
legend {
    padding: 0
}

menu,
ol,
ul {
    list-style: none;
    margin: 0;
    padding: 0
}

dialog {
    padding: 0
}

textarea {
    resize: vertical
}

input::-moz-placeholder,
textarea::-moz-placeholder {
    opacity: 1;
    color: #9ca3af
}

input::placeholder,
textarea::placeholder {
    opacity: 1;
    color: #9ca3af
}

[role=button],
button {
    cursor: pointer
}

:disabled {
    cursor: default
}

audio,
canvas,
embed,
iframe,
img,
object,
svg,
video {
    display: block;
    vertical-align: middle
}

img,
video {
    max-width: 100%;
    height: auto
}

[hidden] {
    display: none
}

*,
::backdrop,
:after,
:before {
    --tw-border-spacing-x: 0;
    --tw-border-spacing-y: 0;
    --tw-translate-x: 0;
    --tw-translate-y: 0;
    --tw-rotate: 0;
    --tw-skew-x: 0;
    --tw-skew-y: 0;
    --tw-scale-x: 1;
    --tw-scale-y: 1;
    --tw-pan-x: ;
    --tw-pan-y: ;
    --tw-pinch-zoom: ;
    --tw-scroll-snap-strictness: proximity;
    --tw-gradient-from-position: ;
    --tw-gradient-via-position: ;
    --tw-gradient-to-position: ;
    --tw-ordinal: ;
    --tw-slashed-zero: ;
    --tw-numeric-figure: ;
    --tw-numeric-spacing: ;
    --tw-numeric-fraction: ;
    --tw-ring-inset: ;
    --tw-ring-offset-width: 0px;
    --tw-ring-offset-color: #fff;
    --tw-ring-color: #3b82f680;
    --tw-ring-offset-shadow: 0 0 #0000;
    --tw-ring-shadow: 0 0 #0000;
    --tw-shadow: 0 0 #0000;
    --tw-shadow-colored: 0 0 #0000;
    --tw-blur: ;
    --tw-brightness: ;
    --tw-contrast: ;
    --tw-grayscale: ;
    --tw-hue-rotate: ;
    --tw-invert: ;
    --tw-saturate: ;
    --tw-sepia: ;
    --tw-drop-shadow: ;
    --tw-backdrop-blur: ;
    --tw-backdrop-brightness: ;
    --tw-backdrop-contrast: ;
    --tw-backdrop-grayscale: ;
    --tw-backdrop-hue-rotate: ;
    --tw-backdrop-invert: ;
    --tw-backdrop-opacity: ;
    --tw-backdrop-saturate: ;
    --tw-backdrop-sepia: ;
    --tw-contain-size: ;
    --tw-contain-layout: ;
    --tw-contain-paint: ;
    --tw-contain-style:
}

.container {
    width: 100%
}

@media (min-width:640px) {
    .container {
        max-width: 640px
    }
}

@media (min-width:768px) {
    .container {
        max-width: 768px
    }
}

@media (min-width:1024px) {
    .container {
        max-width: 1024px
    }
}

@media (min-width:1280px) {
    .container {
        max-width: 1280px
    }
}

@media (min-width:1536px) {
    .container {
        max-width: 1536px
    }
}

.sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border-width: 0
}

.pointer-events-none {
    pointer-events: none
}

.pointer-events-auto {
    pointer-events: auto
}

.visible {
    visibility: visible
}

.static {
    position: static
}

.fixed {
    position: fixed
}

.absolute {
    position: absolute
}

.relative {
    position: relative
}

.inset-0 {
    inset: 0
}

.inset-1 {
    inset: .25rem
}

.-inset-x-12 {
    left: -3rem;
    right: -3rem
}

.inset-x-1 {
    left: .25rem;
    right: .25rem
}

.inset-x-4 {
    left: 1rem;
    right: 1rem
}

.-bottom-0 {
    bottom: 0
}

.-bottom-0\.5 {
    bottom: -.125rem
}

.-bottom-2 {
    bottom: -.5rem
}

.-bottom-6 {
    bottom: -1.5rem
}

.-left-1\/4 {
    left: -25%
}

.-left-2 {
    left: -.5rem
}

.-top-1\/4 {
    top: -25%
}

.-top-4 {
    top: -1rem
}

.-top-6 {
    top: -1.5rem
}

.bottom-0 {
    bottom: 0
}

.bottom-10 {
    bottom: 2.5rem
}

.bottom-6 {
    bottom: 1.5rem
}

.left-0 {
    left: 0
}

.left-1\/2 {
    left: 50%
}

.left-8 {
    left: 2rem
}

.right-4 {
    right: 1rem
}

.top-0 {
    top: 0
}

.top-1\/4 {
    top: 25%
}

.right-0 {
    right: 0
}

.z-30 {
    z-index: 30
}

.z-50 {
    z-index: 50
}

.z-\[-1\] {
    z-index: -1
}

.z-\[500\] {
    z-index: 500
}

.col-span-1 {
    grid-column: span 1/span 1
}

.col-span-10 {
    grid-column: span 10/span 10
}

.col-span-2 {
    grid-column: span 2/span 2
}

.col-span-3 {
    grid-column: span 3/span 3
}

.col-span-4 {
    grid-column: span 4/span 4
}

.col-span-7 {
    grid-column: span 7/span 7
}

.-m-1 {
    margin: -.25rem
}

.-m-1\.5 {
    margin: -.375rem
}

.m-auto {
    margin: auto
}

.mx-auto {
    margin-left: auto;
    margin-right: auto
}

.my-1 {
    margin-top: .25rem;
    margin-bottom: .25rem
}

.my-10 {
    margin-top: 2.5rem;
    margin-bottom: 2.5rem
}

.my-2 {
    margin-top: .5rem;
    margin-bottom: .5rem
}

.my-3 {
    margin-top: .75rem;
    margin-bottom: .75rem
}

.my-4 {
    margin-top: 1rem;
    margin-bottom: 1rem
}

.my-5 {
    margin-top: 1.25rem;
    margin-bottom: 1.25rem
}

.my-6 {
    margin-top: 1.5rem;
    margin-bottom: 1.5rem
}

.-mb-12 {
    margin-bottom: -3rem
}

.-mb-6 {
    margin-bottom: -1.5rem
}

.-ml-2 {
    margin-left: -.5rem
}

.-mr-2 {
    margin-right: -.5rem
}

.-mr-2\.5 {
    margin-right: -.625rem
}

.-mt-1 {
    margin-top: -.25rem
}

.-mt-24 {
    margin-top: -6rem
}

.-mt-3 {
    margin-top: -.75rem
}

.mb-0 {
    margin-bottom: 0
}

.mb-14 {
    margin-bottom: 3.5rem
}

.mb-2 {
    margin-bottom: .5rem
}

.mb-3 {
    margin-bottom: .75rem
}

.mb-4 {
    margin-bottom: 1rem
}

.mb-5 {
    margin-bottom: 1.25rem
}

.mb-7 {
    margin-bottom: 1.75rem
}

.mb-8 {
    margin-bottom: 2rem
}

.mb-9 {
    margin-bottom: 2.25rem
}

.mb-auto {
    margin-bottom: auto
}

.mb-px {
    margin-bottom: 1px
}

.ml-1 {
    margin-left: .25rem
}

.ml-3 {
    margin-left: .75rem
}

.ml-5 {
    margin-left: 1.25rem
}

.mr-2 {
    margin-right: .5rem
}

.mr-3 {
    margin-right: .75rem
}

.mt-1 {
    margin-top: .25rem
}

.mt-1\.5 {
    margin-top: .375rem
}

.mt-10 {
    margin-top: 2.5rem
}

.mt-12 {
    margin-top: 3rem
}

.mt-16 {
    margin-top: 4rem
}

.mt-2 {
    margin-top: .5rem
}

.mt-4 {
    margin-top: 1rem
}

.mt-8 {
    margin-top: 2rem
}

.mt-auto {
    margin-top: auto
}

.block {
    display: block
}

.inline-block {
    display: inline-block
}

.flex {
    display: flex
}

.inline-flex {
    display: inline-flex
}

.table {
    display: table
}

.grid {
    display: grid
}

.hidden {
    display: none
}

.size-2 {
    width: .5rem;
    height: .5rem
}

.size-2\.5 {
    width: .625rem;
    height: .625rem
}

.h-12 {
    height: 3rem
}

.h-2 {
    height: .5rem
}

.h-2\.5 {
    height: .625rem
}

.h-24 {
    height: 6rem
}

.h-32 {
    height: 8rem
}

.h-4 {
    height: 1rem
}

.h-6 {
    height: 1.5rem
}

.h-8 {
    height: 2rem
}

.h-96 {
    height: 24rem
}

.h-\[150\%\] {
    height: 150%
}

.h-full {
    height: 100%
}

.h-px {
    height: 1px
}

.max-h-screen {
    max-height: 100vh
}

.min-h-screen {
    min-height: 100vh
}

.w-0 {
    width: 0
}

.w-11 {
    width: 2.75rem
}

.w-14 {
    width: 3.5rem
}

.w-16 {
    width: 4rem
}

.w-32 {
    width: 8rem
}

.w-4 {
    width: 1rem
}

.w-6 {
    width: 1.5rem
}

.w-60 {
    width: 15rem
}

.w-\[150\%\] {
    width: 150%
}

.w-fit {
    width: -moz-fit-content;
    width: fit-content
}

.w-full {
    width: 100%
}

.w-max {
    width: -moz-max-content;
    width: max-content
}

.w-px {
    width: 1px
}

.min-w-full {
    min-width: 100%
}

.max-w-3xl {
    max-width: 48rem
}

.max-w-5xl {
    max-width: 64rem
}

.max-w-6xl {
    max-width: 72rem
}

.max-w-7xl {
    max-width: 80rem
}

.max-w-\[94\%\] {
    max-width: 94%
}

.max-w-full {
    max-width: 100%
}

.max-w-lg {
    max-width: 32rem
}

.max-w-md {
    max-width: 28rem
}

.max-w-none {
    max-width: none
}

.max-w-sm {
    max-width: 24rem
}

.max-w-xl {
    max-width: 36rem
}

.flex-none {
    flex: none
}

.shrink-0 {
    flex-shrink: 0
}

.table-auto {
    table-layout: auto
}

.border-collapse {
    border-collapse: collapse
}

.origin-bottom {
    transform-origin: bottom
}

.-translate-x-1\/2 {
    --tw-translate-x: -50%
}

.-rotate-3,
.-translate-x-1\/2 {
    transform: translate(var(--tw-translate-x), var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))
}

.-rotate-3 {
    --tw-rotate: -3deg
}

.rotate-3 {
    --tw-rotate: 3deg
}

.rotate-3,
.scale-0 {
    transform: translate(var(--tw-translate-x), var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))
}

.scale-0 {
    --tw-scale-x: 0;
    --tw-scale-y: 0
}

.scale-100 {
    --tw-scale-x: 1;
    --tw-scale-y: 1
}

.scale-100,
.scale-50 {
    transform: translate(var(--tw-translate-x), var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))
}

.scale-50 {
    --tw-scale-x: .5;
    --tw-scale-y: .5
}

.scale-x-0 {
    --tw-scale-x: 0
}

.scale-x-0,
.transform {
    transform: translate(var(--tw-translate-x), var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))
}

.transform-gpu {
    transform: translate3d(var(--tw-translate-x), var(--tw-translate-y), 0) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))
}

@keyframes spin {
    to {
        transform: rotate(1turn)
    }
}

.animate-spin {
    animation: spin 1s linear infinite
}

.cursor-not-allowed {
    cursor: not-allowed
}

.cursor-pointer {
    cursor: pointer
}

.list-disc {
    list-style-type: disc;
}

.list-inside {
    list-style-position: inside;
}

.list-none {
    list-style-type: none
}

.appearance-none {
    -webkit-appearance: none;
    -moz-appearance: none;
    appearance: none
}

.grid-cols-12 {
    grid-template-columns: repeat(12, minmax(0, 1fr))
}

.grid-cols-2 {
    grid-template-columns: repeat(2, minmax(0, 1fr))
}

.grid-cols-3 {
    grid-template-columns: repeat(3, minmax(0, 1fr))
}

.grid-cols-5 {
    grid-template-columns: repeat(5, minmax(0, 1fr))
}

.flex-row {
    flex-direction: row
}

.flex-col {
    flex-direction: column
}

.flex-wrap {
    flex-wrap: wrap
}

.place-items-center {
    place-items: center
}

.items-start {
    align-items: flex-start
}

.items-end {
    align-items: flex-end
}

.items-center {
    align-items: center
}

.justify-end {
    justify-content: flex-end
}

.justify-center {
    justify-content: center
}

.justify-between {
    justify-content: space-between
}

.justify-items-center {
    justify-items: center
}

.gap-0 {
    gap: 0
}

.gap-0\.5 {
    gap: .125rem
}

.gap-10 {
    gap: 2.5rem
}

.gap-12 {
    gap: 3rem
}

.gap-20 {
    gap: 5rem
}

.gap-3 {
    gap: .75rem
}

.gap-4 {
    gap: 1rem
}

.gap-8 {
    gap: 2rem
}

.gap-x-2 {
    -moz-column-gap: .5rem;
    column-gap: .5rem
}

.gap-x-8 {
    -moz-column-gap: 2rem;
    column-gap: 2rem
}

.gap-y-16 {
    row-gap: 4rem
}

.gap-y-4 {
    row-gap: 1rem
}

.space-x-1>:not([hidden])~:not([hidden]) {
    --tw-space-x-reverse: 0;
    margin-right: calc(.25rem*var(--tw-space-x-reverse));
    margin-left: calc(.25rem*(1 - var(--tw-space-x-reverse)))
}

.space-x-2>:not([hidden])~:not([hidden]) {
    --tw-space-x-reverse: 0;
    margin-right: calc(.5rem*var(--tw-space-x-reverse));
    margin-left: calc(.5rem*(1 - var(--tw-space-x-reverse)))
}

.space-x-20>:not([hidden])~:not([hidden]) {
    --tw-space-x-reverse: 0;
    margin-right: calc(5rem*var(--tw-space-x-reverse));
    margin-left: calc(5rem*(1 - var(--tw-space-x-reverse)))
}

.space-x-4>:not([hidden])~:not([hidden]) {
    --tw-space-x-reverse: 0;
    margin-right: calc(1rem*var(--tw-space-x-reverse));
    margin-left: calc(1rem*(1 - var(--tw-space-x-reverse)))
}

.space-x-5>:not([hidden])~:not([hidden]) {
    --tw-space-x-reverse: 0;
    margin-right: calc(1.25rem*var(--tw-space-x-reverse));
    margin-left: calc(1.25rem*(1 - var(--tw-space-x-reverse)))
}

.space-y-2>:not([hidden])~:not([hidden]) {
    --tw-space-y-reverse: 0;
    margin-top: calc(.5rem*(1 - var(--tw-space-y-reverse)));
    margin-bottom: calc(.5rem*var(--tw-space-y-reverse))
}

.space-y-3>:not([hidden])~:not([hidden]) {
    --tw-space-y-reverse: 0;
    margin-top: calc(.75rem*(1 - var(--tw-space-y-reverse)));
    margin-bottom: calc(.75rem*var(--tw-space-y-reverse))
}

.space-y-4>:not([hidden])~:not([hidden]) {
    --tw-space-y-reverse: 0;
    margin-top: calc(1rem*(1 - var(--tw-space-y-reverse)));
    margin-bottom: calc(1rem*var(--tw-space-y-reverse))
}

.space-y-5>:not([hidden])~:not([hidden]) {
    --tw-space-y-reverse: 0;
    margin-top: calc(1.25rem*(1 - var(--tw-space-y-reverse)));
    margin-bottom: calc(1.25rem*var(--tw-space-y-reverse))
}

.space-y-6>:not([hidden])~:not([hidden]) {
    --tw-space-y-reverse: 0;
    margin-top: calc(1.5rem*(1 - var(--tw-space-y-reverse)));
    margin-bottom: calc(1.5rem*var(--tw-space-y-reverse))
}

.divide-y>:not([hidden])~:not([hidden]) {
    --tw-divide-y-reverse: 0;
    border-top-width: calc(1px*(1 - var(--tw-divide-y-reverse)));
    border-bottom-width: calc(1px*var(--tw-divide-y-reverse))
}

.divide-gray-200>:not([hidden])~:not([hidden]) {
    --tw-divide-opacity: 1;
    border-color: rgb(229 231 235/var(--tw-divide-opacity))
}

.justify-self-start {
    justify-self: start
}

.justify-self-end {
    justify-self: end
}

.overflow-hidden {
    overflow: hidden
}

.overflow-visible {
    overflow: visible
}

.overflow-x-auto {
    overflow-x: auto
}

.whitespace-normal {
    white-space: normal
}

.whitespace-nowrap {
    white-space: nowrap
}

.text-wrap {
    text-wrap: wrap
}

.rounded {
    border-radius: .25rem
}

.rounded-2xl {
    border-radius: 1rem
}

.rounded-3xl {
    border-radius: 1.5rem
}

.rounded-full {
    border-radius: 9999px
}

.rounded-lg {
    border-radius: .5rem
}

.rounded-md {
    border-radius: .375rem
}

.rounded-xl {
    border-radius: .75rem
}

.rounded-l-\[20px\] {
    border-top-left-radius: 20px;
    border-bottom-left-radius: 20px
}

.rounded-r-\[20px\] {
    border-top-right-radius: 20px;
    border-bottom-right-radius: 20px
}

.rounded-r-\[8px\] {
    border-top-right-radius: 8px;
    border-bottom-right-radius: 8px
}

.rounded-t-full {
    border-top-left-radius: 9999px;
    border-top-right-radius: 9999px
}

.border {
    border-width: 1px
}

.border-x-0 {
    border-left-width: 0;
    border-right-width: 0
}

.border-b {
    border-bottom-width: 1px
}

.border-b-0 {
    border-bottom-width: 0
}

.border-b-2 {
    border-bottom-width: 2px
}

.border-l-0 {
    border-left-width: 0
}

.border-r-0 {
    border-right-width: 0
}

.border-t-0 {
    border-top-width: 0
}

.border-t-1 {
    border-top-width: 1px
}

.border-none {
    border-style: none
}

.border-\[\#D7DFE9\] {
    --tw-border-opacity: 1;
    border-color: rgb(215 223 233/var(--tw-border-opacity))
}

.border-gray-300 {
    --tw-border-opacity: 1;
    border-color: rgb(209 213 219/var(--tw-border-opacity))
}

.border-gray-400 {
    --tw-border-opacity: 1;
    border-color: rgb(156 163 175/var(--tw-border-opacity))
}

.border-primary-200 {
    --tw-border-opacity: 1;
    border-color: rgb(171 228 214/var(--tw-border-opacity))
}

.border-primary-400 {
    --tw-border-opacity: 1;
    border-color: rgb(78 179 161/var(--tw-border-opacity))
}

.border-primary-500 {
    --tw-border-opacity: 1;
    border-color: rgb(54 156 140/var(--tw-border-opacity))
}

.border-primary-600 {
    --tw-border-opacity: 1;
    border-color: rgb(40 121 110/var(--tw-border-opacity))
}

.border-primary-700 {
    --tw-border-opacity: 1;
    border-color: rgb(35 98 90/var(--tw-border-opacity))
}

.border-secondary-400 {
    --tw-border-opacity: 1;
    border-color: rgb(255 121 100/var(--tw-border-opacity))
}

.border-secondary-500 {
    --tw-border-opacity: 1;
    border-color: rgb(255 78 51/var(--tw-border-opacity))
}

.border-transparent {
    border-color: #0000
}

.border-y-secondary-300 {
    --tw-border-opacity: 1;
    border-top-color: rgb(255 170 157/var(--tw-border-opacity));
    border-bottom-color: rgb(255 170 157/var(--tw-border-opacity))
}

.bg-black\/20 {
    background-color: #0003
}

.bg-black-molly {
    --tw-bg-opacity: 1;
    background-color: #0B0D12;
    color: #FFFFFF
}

.bg-gray-100 {
    --tw-bg-opacity: 1;
    background-color: rgb(243 244 246/var(--tw-bg-opacity))
}

.bg-gray-50 {
    --tw-bg-opacity: 1;
    background-color: rgb(249 250 251/var(--tw-bg-opacity))
}

.bg-gray-500\/25 {
    background-color: #6b728040
}

.bg-primary-100 {
    --tw-bg-opacity: 1;
    background-color: rgb(213 242 235/var(--tw-bg-opacity))
}

.bg-primary-200 {
    --tw-bg-opacity: 1;
    background-color: rgb(171 228 214/var(--tw-bg-opacity))
}

.bg-primary-300 {
    --tw-bg-opacity: 1;
    background-color: rgb(122 206 189/var(--tw-bg-opacity))
}

.bg-primary-50 {
    --tw-bg-opacity: 1;
    background-color: rgb(243 250 249/var(--tw-bg-opacity))
}

.bg-primary-500 {
    --tw-bg-opacity: 1;
    background-color: rgb(54 156 140/var(--tw-bg-opacity))
}

.bg-primary-800 {
    --tw-bg-opacity: 1;
    background-color: rgb(32 79 74/var(--tw-bg-opacity))
}

.bg-primary-950 {
    --tw-bg-opacity: 1;
    background-color: rgb(13 38 37/var(--tw-bg-opacity))
}

.bg-secondary-300 {
    --tw-bg-opacity: 1;
    background-color: rgb(255 170 157/var(--tw-bg-opacity))
}

.bg-secondary-500 {
    --tw-bg-opacity: 1;
    background-color: rgb(255 78 51/var(--tw-bg-opacity))
}

.bg-secondary-700 {
    --tw-bg-opacity: 1;
    background-color: rgb(200 38 13/var(--tw-bg-opacity))
}

.bg-transparent {
    background-color: initial
}

.bg-opacity-0 {
    --tw-bg-opacity: 0
}

.bg-gradient-to-b {
    background-image: linear-gradient(to bottom, var(--tw-gradient-stops))
}

.bg-gradient-to-br {
    background-image: linear-gradient(to bottom right, var(--tw-gradient-stops))
}

.bg-gradient-to-r {
    background-image: linear-gradient(to right, var(--tw-gradient-stops))
}

.bg-gradient-to-t {
    background-image: linear-gradient(to top, var(--tw-gradient-stops))
}

.from-gray-100 {
    --tw-gradient-from: #f3f4f6 var(--tw-gradient-from-position);
    --tw-gradient-to: #f3f4f600 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), var(--tw-gradient-to)
}

.from-primary-900\/50 {
    --tw-gradient-from: #1f423e80 var(--tw-gradient-from-position);
    --tw-gradient-to: #1f423e00 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), var(--tw-gradient-to)
}

.from-primary-950 {
    --tw-gradient-from: #0d2625 var(--tw-gradient-from-position);
    --tw-gradient-to: #0d262500 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), var(--tw-gradient-to)
}

.from-transparent {
    --tw-gradient-from: #0000 var(--tw-gradient-from-position);
    --tw-gradient-to: #0000 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), var(--tw-gradient-to)
}

.from-10\% {
    --tw-gradient-from-position: 10%
}

.via-primary-900 {
    --tw-gradient-to: #1f423e00 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), #1f423e var(--tw-gradient-via-position), var(--tw-gradient-to)
}

.via-primary-900\/50 {
    --tw-gradient-to: #1f423e00 var(--tw-gradient-to-position);
    --tw-gradient-stops: var(--tw-gradient-from), #1f423e80 var(--tw-gradient-via-position), var(--tw-gradient-to)
}

.via-10\% {
    --tw-gradient-via-position: 10%
}

.to-primary-950 {
    --tw-gradient-to: #0d2625 var(--tw-gradient-to-position)
}

.to-primary-950\/50 {
    --tw-gradient-to: #0d262580 var(--tw-gradient-to-position)
}

.to-transparent {
    --tw-gradient-to: #0000 var(--tw-gradient-to-position)
}

.to-90\% {
    --tw-gradient-to-position: 90%
}

.bg-\[length\:100px_auto\] {
    background-size: 100px auto
}

.bg-cover {
    background-size: cover
}

.bg-clip-padding {
    background-clip: padding-box
}

.bg-center {
    background-position: 50%
}

.fill-primary-400 {
    fill: #4eb3a1
}

.object-contain {
    -o-object-fit: contain;
    object-fit: contain
}

.object-cover {
    -o-object-fit: cover;
    object-fit: cover
}

.object-bottom {
    -o-object-position: bottom;
    object-position: bottom
}

.p-0 {
    padding: 0
}

.p-1 {
    padding: .25rem
}

.p-1\.5 {
    padding: .375rem
}

.p-10 {
    padding: 2.5rem
}

.p-16 {
    padding: 4rem
}

.p-2 {
    padding: .5rem
}

.p-4 {
    padding: 1rem
}

.p-5 {
    padding: 1.25rem
}

.p-6 {
    padding: 1.5rem
}

.px-10 {
    padding-left: 2.5rem;
    padding-right: 2.5rem
}

.px-2 {
    padding-left: .5rem;
    padding-right: .5rem
}

.px-3 {
    padding-left: .75rem;
    padding-right: .75rem
}

.px-4 {
    padding-left: 1rem;
    padding-right: 1rem
}

.px-40 {
    padding-left: 10rem;
    padding-right: 10rem
}

.px-5 {
    padding-left: 1.25rem;
    padding-right: 1.25rem
}

.px-6 {
    padding-left: 1.5rem;
    padding-right: 1.5rem
}

.px-7 {
    padding-left: 1.75rem;
    padding-right: 1.75rem
}

.py-0 {
    padding-top: 0;
    padding-bottom: 0
}

.py-1 {
    padding-top: .25rem;
    padding-bottom: .25rem
}

.py-10 {
    padding-top: 2.5rem;
    padding-bottom: 2.5rem
}

.py-16 {
    padding-top: 4rem;
    padding-bottom: 4rem
}

.py-2 {
    padding-top: .5rem;
    padding-bottom: .5rem
}

.py-2\.5 {
    padding-top: .625rem;
    padding-bottom: .625rem
}

.py-24 {
    padding-top: 6rem;
    padding-bottom: 6rem
}

.py-3 {
    padding-top: .75rem;
    padding-bottom: .75rem
}

.py-4 {
    padding-top: 1rem;
    padding-bottom: 1rem
}

.py-5 {
    padding-top: 1.25rem;
    padding-bottom: 1.25rem
}

.py-6 {
    padding-top: 1.5rem;
    padding-bottom: 1.5rem
}

.py-8 {
    padding-top: 2rem;
    padding-bottom: 2rem
}

.pb-12 {
    padding-bottom: 3rem
}

.pb-16 {
    padding-bottom: 4rem
}

.pb-3 {
    padding-bottom: .75rem
}

.pb-3\.5 {
    padding-bottom: .875rem
}

.pb-32 {
    padding-bottom: 8rem
}

.pb-8 {
    padding-bottom: 2rem
}

.pl-3 {
    padding-left: .75rem
}

.pl-4 {
    padding-left: 1rem
}

.pr-3 {
    padding-right: .75rem
}

.pr-7 {
    padding-right: 1.75rem
}

.pr-\[10px\] {
    padding-right: 10px
}

.pt-12 {
    padding-top: 3rem
}

.pt-24 {
    padding-top: 6rem
}

.pt-4 {
    padding-top: 1rem
}

.underline {
    text-decoration: underline
}

.text-left {
    text-align: left
}

.text-center {
    text-align: center
}

.text-right {
    text-align: right
}

.text-start {
    text-align: start
}

.align-middle {
    vertical-align: middle
}

.font-mono {
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace
}

.text-2xl {
    font-size: 1.5rem;
    line-height: 2rem
}

.text-3xl {
    font-size: 1.875rem;
    line-height: 2.25rem
}

.text-4xl {
    font-size: 2.25rem;
    line-height: 2.5rem
}

.text-5xl {
    font-size: 3rem;
    line-height: 1
}

.text-7xl {
    font-size: 4.5rem;
    line-height: 1
}

.text-base {
    font-size: 1rem;
    line-height: 1.5rem
}

.text-lg {
    font-size: 1.125rem;
    line-height: 1.75rem
}

.text-sm {
    font-size: .875rem;
    line-height: 1.25rem
}

.text-xl {
    font-size: 1.25rem;
    line-height: 1.75rem
}

.text-xs {
    font-size: .75rem;
    line-height: 1rem
}

.font-\[450\] {
    font-weight: 450
}

.font-bold {
    font-weight: 700
}

.font-medium {
    font-weight: 500
}

.font-semibold {
    font-weight: 600
}

.uppercase {
    text-transform: uppercase
}

.lowercase {
    text-transform: lowercase
}

.leading-6 {
    line-height: 1.5rem
}

.leading-none {
    line-height: 1
}

.leading-tight {
    line-height: 1.25
}

.tracking-tight {
    letter-spacing: -.025em
}

.tracking-wide {
    letter-spacing: .025em
}

.tracking-wider {
    letter-spacing: .05em
}

.text-gray-100 {
    --tw-text-opacity: 1;
    color: rgb(243 244 246/var(--tw-text-opacity))
}

.text-gray-200 {
    --tw-text-opacity: 1;
    color: rgb(229 231 235/var(--tw-text-opacity))
}

.text-gray-300 {
    --tw-text-opacity: 1;
    color: rgb(209 213 219/var(--tw-text-opacity))
}

.text-gray-50 {
    --tw-text-opacity: 1;
    color: rgb(249 250 251/var(--tw-text-opacity))
}

.text-gray-500 {
    --tw-text-opacity: 1;
    color: rgb(107 114 128/var(--tw-text-opacity))
}

.text-gray-600 {
    --tw-text-opacity: 1;
    color: rgb(75 85 99/var(--tw-text-opacity))
}

.text-gray-700 {
    --tw-text-opacity: 1;
    color: rgb(55 65 81/var(--tw-text-opacity))
}

.text-gray-800 {
    --tw-text-opacity: 1;
    color: rgb(31 41 55/var(--tw-text-opacity))
}

.text-gray-900 {
    --tw-text-opacity: 1;
    color: rgb(17 24 39/var(--tw-text-opacity))
}

.text-primary-300 {
    --tw-text-opacity: 1;
    color: rgb(122 206 189/var(--tw-text-opacity))
}

.text-primary-400 {
    --tw-text-opacity: 1;
    color: rgb(78 179 161/var(--tw-text-opacity))
}

.text-primary-50 {
    --tw-text-opacity: 1;
    color: rgb(243 250 249/var(--tw-text-opacity))
}

.text-primary-500 {
    --tw-text-opacity: 1;
    color: rgb(54 156 140/var(--tw-text-opacity))
}

.text-primary-600 {
    --tw-text-opacity: 1;
    color: rgb(40 121 110/var(--tw-text-opacity))
}

.text-primary-800 {
    --tw-text-opacity: 1;
    color: rgb(32 79 74/var(--tw-text-opacity))
}

.text-primary-900 {
    --tw-text-opacity: 1;
    color: rgb(31 66 62/var(--tw-text-opacity))
}

.text-primary-950 {
    --tw-text-opacity: 1;
    color: rgb(13 38 37/var(--tw-text-opacity))
}

.text-secondary-300 {
    --tw-text-opacity: 1;
    color: rgb(255 170 157/var(--tw-text-opacity))
}

.text-secondary-50 {
    --tw-text-opacity: 1;
    color: rgb(255 243 241/var(--tw-text-opacity))
}

.text-secondary-500 {
    --tw-text-opacity: 1;
    color: rgb(255 78 51/var(--tw-text-opacity))
}

.text-secondary-600 {
    --tw-text-opacity: 1;
    color: rgb(237 50 21/var(--tw-text-opacity))
}

.text-secondary-700 {
    --tw-text-opacity: 1;
    color: rgb(200 38 13/var(--tw-text-opacity))
}

.accent-primary-500 {
    accent-color: #369c8c
}

.opacity-0 {
    opacity: 0
}

.opacity-100 {
    opacity: 1
}

.opacity-50 {
    opacity: .5
}

.shadow {
    --tw-shadow: 0 1px 3px 0 #0000001a, 0 1px 2px -1px #0000001a;
    --tw-shadow-colored: 0 1px 3px 0 var(--tw-shadow-color), 0 1px 2px -1px var(--tw-shadow-color)
}

.shadow,
.shadow-lg {
    box-shadow: var(--tw-ring-offset-shadow, 0 0 #0000), var(--tw-ring-shadow, 0 0 #0000), var(--tw-shadow)
}

.shadow-lg {
    --tw-shadow: 0 10px 15px -3px #0000001a, 0 4px 6px -4px #0000001a;
    --tw-shadow-colored: 0 10px 15px -3px var(--tw-shadow-color), 0 4px 6px -4px var(--tw-shadow-color)
}

.shadow-md {
    --tw-shadow: 0 4px 6px -1px #0000001a, 0 2px 4px -2px #0000001a;
    --tw-shadow-colored: 0 4px 6px -1px var(--tw-shadow-color), 0 2px 4px -2px var(--tw-shadow-color)
}

.shadow-md,
.shadow-sm {
    box-shadow: var(--tw-ring-offset-shadow, 0 0 #0000), var(--tw-ring-shadow, 0 0 #0000), var(--tw-shadow)
}

.shadow-sm {
    --tw-shadow: 0 1px 2px 0 #0000000d;
    --tw-shadow-colored: 0 1px 2px 0 var(--tw-shadow-color)
}

.shadow-gray-800\/5 {
    --tw-shadow-color: #1f29370d;
    --tw-shadow: var(--tw-shadow-colored)
}

.outline-0 {
    outline-width: 0
}

.ring-1 {
    --tw-ring-offset-shadow: var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) var(--tw-ring-offset-color);
    --tw-ring-shadow: var(--tw-ring-inset) 0 0 0 calc(1px + var(--tw-ring-offset-width)) var(--tw-ring-color);
    box-shadow: var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow, 0 0 #0000)
}

.ring-inset {
    --tw-ring-inset: inset
}

.ring-gray-800\/\[\.075\] {
    --tw-ring-color: rgba(31, 41, 55, .075)
}

.ring-primary-100 {
    --tw-ring-opacity: 1;
    --tw-ring-color: rgb(213 242 235/var(--tw-ring-opacity))
}

.ring-primary-200 {
    --tw-ring-opacity: 1;
    --tw-ring-color: rgb(171 228 214/var(--tw-ring-opacity))
}

.blur {
    --tw-blur: blur(8px)
}

.blur,
.blur-3xl {
    filter: var(--tw-blur) var(--tw-brightness) var(--tw-contrast) var(--tw-grayscale) var(--tw-hue-rotate) var(--tw-invert) var(--tw-saturate) var(--tw-sepia) var(--tw-drop-shadow)
}

.blur-3xl {
    --tw-blur: blur(64px)
}

.brightness-0 {
    --tw-brightness: brightness(0)
}

.brightness-0,
.drop-shadow {
    filter: var(--tw-blur) var(--tw-brightness) var(--tw-contrast) var(--tw-grayscale) var(--tw-hue-rotate) var(--tw-invert) var(--tw-saturate) var(--tw-sepia) var(--tw-drop-shadow)
}

.drop-shadow {
    --tw-drop-shadow: drop-shadow(0 1px 2px #0000001a) drop-shadow(0 1px 1px #0000000f)
}

.drop-shadow-sm {
    --tw-drop-shadow: drop-shadow(0 1px 1px #0000000d)
}

.drop-shadow-sm,
.invert {
    filter: var(--tw-blur) var(--tw-brightness) var(--tw-contrast) var(--tw-grayscale) var(--tw-hue-rotate) var(--tw-invert) var(--tw-saturate) var(--tw-sepia) var(--tw-drop-shadow)
}

.invert {
    --tw-invert: invert(100%)
}

.filter {
    filter: var(--tw-blur) var(--tw-brightness) var(--tw-contrast) var(--tw-grayscale) var(--tw-hue-rotate) var(--tw-invert) var(--tw-saturate) var(--tw-sepia) var(--tw-drop-shadow)
}

.backdrop-blur-md {
    --tw-backdrop-blur: blur(12px)
}

.backdrop-blur-md,
.backdrop-blur-sm {
    -webkit-backdrop-filter: var(--tw-backdrop-blur) var(--tw-backdrop-brightness) var(--tw-backdrop-contrast) var(--tw-backdrop-grayscale) var(--tw-backdrop-hue-rotate) var(--tw-backdrop-invert) var(--tw-backdrop-opacity) var(--tw-backdrop-saturate) var(--tw-backdrop-sepia);
    backdrop-filter: var(--tw-backdrop-blur) var(--tw-backdrop-brightness) var(--tw-backdrop-contrast) var(--tw-backdrop-grayscale) var(--tw-backdrop-hue-rotate) var(--tw-backdrop-invert) var(--tw-backdrop-opacity) var(--tw-backdrop-saturate) var(--tw-backdrop-sepia)
}

.backdrop-blur-sm {
    --tw-backdrop-blur: blur(4px)
}

.backdrop-blur-xl {
    --tw-backdrop-blur: blur(24px);
    -webkit-backdrop-filter: var(--tw-backdrop-blur) var(--tw-backdrop-brightness) var(--tw-backdrop-contrast) var(--tw-backdrop-grayscale) var(--tw-backdrop-hue-rotate) var(--tw-backdrop-invert) var(--tw-backdrop-opacity) var(--tw-backdrop-saturate) var(--tw-backdrop-sepia);
    backdrop-filter: var(--tw-backdrop-blur) var(--tw-backdrop-brightness) var(--tw-backdrop-contrast) var(--tw-backdrop-grayscale) var(--tw-backdrop-hue-rotate) var(--tw-backdrop-invert) var(--tw-backdrop-opacity) var(--tw-backdrop-saturate) var(--tw-backdrop-sepia)
}

.transition {
    transition-property: color, background-color, border-color, text-decoration-color, fill, stroke, opacity, box-shadow, transform, filter, -webkit-backdrop-filter;
    transition-property: color, background-color, border-color, text-decoration-color, fill, stroke, opacity, box-shadow, transform, filter, backdrop-filter;
    transition-property: color, background-color, border-color, text-decoration-color, fill, stroke, opacity, box-shadow, transform, filter, backdrop-filter, -webkit-backdrop-filter;
    transition-timing-function: cubic-bezier(.4, 0, .2, 1);
    transition-duration: .15s
}

.transition-all {
    transition-property: all;
    transition-timing-function: cubic-bezier(.4, 0, .2, 1);
    transition-duration: .15s
}

.transition-colors {
    transition-property: color, background-color, border-color, text-decoration-color, fill, stroke;
    transition-timing-function: cubic-bezier(.4, 0, .2, 1);
    transition-duration: .15s
}

.transition-opacity {
    transition-property: opacity;
    transition-timing-function: cubic-bezier(.4, 0, .2, 1);
    transition-duration: .15s
}

.delay-100 {
    transition-delay: .1s
}

.delay-150 {
    transition-delay: .15s
}

.duration-200 {
    transition-duration: .2s
}

.duration-300 {
    transition-duration: .3s
}

.duration-500 {
    transition-duration: .5s
}

.ease-out {
    transition-timing-function: cubic-bezier(0, 0, .2, 1)
}

.ease-in-out {
    transition-timing-function: cubic-bezier(.4, 0, .2, 1)
}

.\[-webkit-mask-image\:linear-gradient\(to_bottom\2c rgba\(255\2c 255\2c 255\2c 1\)_75\%\2c rgba\(255\2c 255\2c 255\2c 0\)\)\] {
    -webkit-mask-image: linear-gradient(180deg, #fff 75%, #fff0)
}

.\[-webkit-mask-image\:linear-gradient\(to_top\2c rgba\(255\2c 255\2c 255\2c 1\)_75\%\2c rgba\(255\2c 255\2c 255\2c 0\)\)\] {
    -webkit-mask-image: linear-gradient(0deg, #fff 75%, #fff0)
}

.after\:absolute:after {
    content: var(--tw-content);
    position: absolute
}

.after\:bottom-0:after {
    content: var(--tw-content);
    bottom: 0
}

.after\:left-\[0\.0625rem\]:after {
    content: var(--tw-content);
    left: .0625rem
}

.after\:top-0:after {
    content: var(--tw-content);
    top: 0
}

.after\:my-auto:after {
    content: var(--tw-content);
    margin-top: auto;
    margin-bottom: auto
}

.after\:h-5:after {
    content: var(--tw-content);
    height: 1.25rem
}

.after\:w-5:after {
    content: var(--tw-content);
    width: 1.25rem
}

.after\:rounded-full:after {
    content: var(--tw-content);
    border-radius: 9999px
}

.after\:bg-gray-600:after {
    content: var(--tw-content);
    --tw-bg-opacity: 1;
    background-color: rgb(75 85 99/var(--tw-bg-opacity))
}

.after\:transition-all:after {
    content: var(--tw-content);
    transition-property: all;
    transition-timing-function: cubic-bezier(.4, 0, .2, 1);
    transition-duration: .15s
}

.after\:content-\[\'\'\]:after{--tw-content:"";content:var(--tw-content)}.hover\:isolate:hover{isolation:isolate}.hover\:border-primary-200:hover{--tw-border-opacity:1;border-color:rgb(171 228 214/var(--tw-border-opacity))}.hover\:border-transparent:hover{border-color:#0000}.hover\:bg-gray-200:hover{--tw-bg-opacity:1;background-color:rgb(229 231 235/var(--tw-bg-opacity))}.hover\:bg-primary-100:hover{--tw-bg-opacity:1;background-color:rgb(213 242 235/var(--tw-bg-opacity))}.hover\:bg-primary-700:hover{--tw-bg-opacity:1;background-color:rgb(35 98 90/var(--tw-bg-opacity))}.hover\:bg-primary-800:hover{--tw-bg-opacity:1;background-color:rgb(32 79 74/var(--tw-bg-opacity))}.hover\:bg-secondary-100:hover{--tw-bg-opacity:1;background-color:rgb(255 227 223/var(--tw-bg-opacity))}.hover\:bg-secondary-700:hover{--tw-bg-opacity:1;background-color:rgb(200 38 13/var(--tw-bg-opacity))}.hover\:bg-secondary-800:hover{--tw-bg-opacity:1;background-color:rgb(165 35 15/var(--tw-bg-opacity))}.hover\:bg-opacity-50:hover{--tw-bg-opacity:0.5}.hover\:font-bold:hover{font-weight:700}.hover\:text-gray-50:hover{--tw-text-opacity:1;color:rgb(249 250 251/var(--tw-text-opacity))}.hover\:text-primary-400:hover{--tw-text-opacity:1;color:rgb(78 179 161/var(--tw-text-opacity))}.hover\:text-primary-50:hover{--tw-text-opacity:1;color:rgb(243 250 249/var(--tw-text-opacity))}.hover\:text-primary-500:hover{--tw-text-opacity:1;color:rgb(54 156 140/var(--tw-text-opacity))}.hover\:text-primary-700:hover{--tw-text-opacity:1;color:rgb(35 98 90/var(--tw-text-opacity))}.hover\:text-primary-800:hover{--tw-text-opacity:1;color:rgb(32 79 74/var(--tw-text-opacity))}.hover\:text-secondary-800:hover{--tw-text-opacity:1;color:rgb(165 35 15/var(--tw-text-opacity))}.hover\:opacity-75:hover{opacity:.75}.focus\:isolate:focus{isolation:isolate}.focus\:border-primary-300:focus{--tw-border-opacity:1;border-color:rgb(122 206 189/var(--tw-border-opacity))}.focus\:border-primary-500:focus{--tw-border-opacity:1;border-color:rgb(54 156 140/var(--tw-border-opacity))}.focus\:border-transparent:focus{border-color:#0000}.focus\:bg-opacity-50:focus{--tw-bg-opacity:0.5}.focus\:text-primary-800:focus{--tw-text-opacity:1;color:rgb(32 79 74/var(--tw-text-opacity))}.focus\:outline-none:focus{outline:2px solid #0000;outline-offset:2px}.focus\:ring-\[1px\]:focus{--tw-ring-offset-shadow:var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) var(--tw-ring-offset-color);--tw-ring-shadow:var(--tw-ring-inset) 0 0 0 calc(1px + var(--tw-ring-offset-width)) var(--tw-ring-color)}.focus\:ring-\[1px\]:focus,.focus\:ring-\[3px\]:focus{box-shadow:var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow,0 0 #0000)}.focus\:ring-\[3px\]:focus{--tw-ring-offset-shadow:var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) var(--tw-ring-offset-color);--tw-ring-shadow:var(--tw-ring-inset) 0 0 0 calc(3px + var(--tw-ring-offset-width)) var(--tw-ring-color)}.focus\:ring-primary-200:focus{--tw-ring-opacity:1;--tw-ring-color:rgb(171 228 214/var(--tw-ring-opacity))}.focus-visible\:outline:focus-visible{outline-style:solid}.focus-visible\:outline-2:focus-visible{outline-width:2px}.focus-visible\:outline-offset-2:focus-visible{outline-offset:2px}.focus-visible\:outline-black:focus-visible{outline-color:#000}.active\:opacity-100:active{opacity:1}.active\:outline-offset-0:active{outline-offset:0}.disabled\:pointer-events-none:disabled{pointer-events:none}.disabled\:opacity-50:disabled{opacity:.5}.group\/btn:hover .group-hover\/btn\:w-2{width:.5rem}.group\/btn:hover .group-hover\/btn\:w-2\.5{width:.625rem}.group:hover .group-hover\:scale-100{--tw-scale-x:1;--tw-scale-y:1}.group:hover .group-hover\:scale-100,.group:hover .group-hover\:scale-x-100{transform:translate(var(--tw-translate-x),var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))}.group:hover .group-hover\:scale-x-100{--tw-scale-x:1}.group:hover .group-hover\:opacity-100,.group\/btn:hover .group-hover\/btn\:opacity-100{opacity:1}.peer:checked~.peer-checked\:bg-primary-500{--tw-bg-opacity:1;background-color:rgb(54 156 140/var(--tw-bg-opacity))}.peer:checked~.peer-checked\:text-gray-900{--tw-text-opacity:1;color:rgb(17 24 39/var(--tw-text-opacity))}.peer:checked~.peer-checked\:after\:translate-x-5:after{content:var(--tw-content);--tw-translate-x:1.25rem;transform:translate(var(--tw-translate-x),var(--tw-translate-y)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))}.peer:focus~.peer-focus\:outline{outline-style:solid}.peer:focus~.peer-focus\:outline-2{outline-width:2px}.peer:focus~.peer-focus\:outline-offset-2{outline-offset:2px}.peer:focus~.peer-focus\:outline-gray-800{outline-color:#1f2937}.peer:focus:checked~.peer-focus\:peer-checked\:outline-primary-500{outline-color:#369c8c}.peer:active~.peer-active\:outline-offset-0{outline-offset:0}.peer:disabled~.peer-disabled\:cursor-not-allowed{cursor:not-allowed}.peer:disabled~.peer-disabled\:opacity-70{opacity:.7}@media (prefers-reduced-motion:reduce){.motion-reduce\:transition-opacity{transition-property:opacity;transition-timing-function:cubic-bezier(.4,0,.2,1);transition-duration:.15s}}@media (min-width:640px){.sm\:left-14{left:3.5rem}.sm\:w-auto{width:auto}.sm\:grid-cols-3{grid-template-columns:repeat(3,minmax(0,1fr))}.sm\:items-center{align-items:center}.sm\:px-6{padding-left:1.5rem;padding-right:1.5rem}.sm\:pb-40{padding-bottom:10rem}.sm\:pl-20{padding-left:5rem}.sm\:text-3xl{font-size:1.875rem;line-height:2.25rem}.sm\:text-5xl{font-size:3rem;line-height:1}.sm\:text-sm{font-size:.875rem;line-height:1.25rem}}@media (min-width:768px){.md\:block{display:block}.md\:h-screen{height:100vh}.md\:w-16{width:4rem}.md\:w-20{width:5rem}.md\:w-24{width:6rem}.md\:max-w-2xl{max-width:42rem}.md\:grid-cols-3{grid-template-columns:repeat(3,minmax(0,1fr))}.md\:grid-cols-5{grid-template-columns:repeat(5,minmax(0,1fr))}.md\:gap-16{gap:4rem}.md\:px-14{padding-left:3.5rem;padding-right:3.5rem}.md\:py-10{padding-top:2.5rem;padding-bottom:2.5rem}.md\:pb-52{padding-bottom:13rem}.md\:text-3xl{font-size:1.875rem;line-height:2.25rem}.md\:text-4xl{font-size:2.25rem;line-height:2.5rem}.md\:text-xl{font-size:1.25rem;line-height:1.75rem}}@media (min-width:1024px){.lg\:left-20{left:5rem}.lg\:order-last{order:9999}.lg\:col-span-3{grid-column:span 3/span 3}.lg\:col-span-4{grid-column:span 4/span 4}.lg\:-my-11{margin-top:-2.75rem;margin-bottom:-2.75rem}.lg\:-mr-9{margin-right:-2.25rem}.lg\:mb-0{margin-bottom:0}.lg\:mb-16{margin-bottom:4rem}.lg\:mt-0{margin-top:0}.lg\:block{display:block}.lg\:flex{display:flex}.lg\:grid{display:grid}.lg\:max-w-4xl{max-width:56rem}.lg\:max-w-7xl{max-width:80rem}.lg\:max-w-max{max-width:-moz-max-content;max-width:max-content}.lg\:max-w-xl{max-width:36rem}.lg\:grid-cols-2{grid-template-columns:repeat(2,minmax(0,1fr))}.lg\:grid-cols-3{grid-template-columns:repeat(3,minmax(0,1fr))}.lg\:grid-cols-7{grid-template-columns:repeat(7,minmax(0,1fr))}.lg\:gap-5{gap:1.25rem}.lg\:p-10{padding:2.5rem}.lg\:p-8{padding:2rem}.lg\:px-8{padding-left:2rem;padding-right:2rem}.lg\:py-12{padding-top:3rem;padding-bottom:3rem}.lg\:py-24{padding-top:6rem;padding-bottom:6rem}.lg\:pb-20{padding-bottom:5rem}.lg\:pb-60{padding-bottom:15rem}.lg\:pl-28{padding-left:7rem}.lg\:pt-20{padding-top:5rem}.lg\:pt-40{padding-top:10rem}.lg\:text-4xl{font-size:2.25rem;line-height:2.5rem}.lg\:text-5xl{font-size:3rem;line-height:1}.lg\:text-6xl{font-size:3.75rem;line-height:1}.lg\:text-lg{font-size:1.125rem;line-height:1.75rem}}@media (min-width:1280px){.xl\:col-span-3{grid-column:span 3/span 3}.xl\:mx-auto{margin-left:auto;margin-right:auto}.xl\:flex{display:flex}.xl\:hidden{display:none}.xl\:w-auto{width:auto}.xl\:w-full{width:100%}.xl\:max-w-lg{max-width:32rem}.xl\:max-w-xl{max-width:36rem}.xl\:gap-16{gap:4rem}.xl\:gap-x-16{-moz-column-gap:4rem;column-gap:4rem}.xl\:px-20{padding-left:5rem;padding-right:5rem}.xl\:py-20{padding-top:5rem;padding-bottom:5rem}.xl\:py-32{padding-top:8rem}.xl\:pb-32,.xl\:py-32{padding-bottom:8rem}.xl\:pb-\[16\.5rem\]{padding-bottom:16.5rem}.xl\:text-xl{font-size:1.25rem;line-height:1.75rem}}@media (prefers-color-scheme:dark){.dark\:border-gray-700{--tw-border-opacity:1;border-color:rgb(55 65 81/var(--tw-border-opacity))}.dark\:bg-gray-900{--tw-bg-opacity:1;background-color:rgb(17 24 39/var(--tw-bg-opacity))}.dark\:text-gray-300{--tw-text-opacity:1;color:rgb(209 213 219/var(--tw-text-opacity))}.dark\:after\:bg-gray-300:after{content:var(--tw-content);--tw-bg-opacity:1;background-color:rgb(209 213 219/var(--tw-bg-opacity))}.peer:checked~.dark\:peer-checked\:bg-primary-500{--tw-bg-opacity:1;background-color:rgb(54 156 140/var(--tw-bg-opacity))}.peer:focus~.dark\:peer-focus\:outline-gray-300{outline-color:#d1d5db}.peer:focus:checked~.dark\:peer-focus\:peer-checked\:outline-primary-500{outline-color:#369c8c}}
|}
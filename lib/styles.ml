let static_css = Tyxml.Html.Unsafe.data {|
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

#robur-logo {
    transform: translate(-35%, 100%) rotate(270deg);
    position: fixed;
}

@media (max-width: 767px) {
    #robur-logo {
        display: none !important; /* Hide on small screens */
    }
}
@media (max-height: 740px) {
    #robur-logo {
        display: none !important; /* Hide on small screens */
    }
}

#robur-logo {
    transform: translate(-20%, 100%) rotate(270deg);
    width: 60em;
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
	font-weight: inherit;
	margin-top: 1rem;
	margin-bottom: 1rem;
	font-size: 1.25rem;
	line-height: 1.75rem;
	font-weight: 600;
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

table {
	text-indent: 0;
	border-color: inherit;
	border-collapse: collapse;
	table-layout: auto;
	min-width: 100%;
	margin-top: 1rem;
	margin-bottom: 1rem
}

th {

	margin-top: 1.5rem;
	margin-bottom: 1.5rem;
	padding-top: 1rem;
	padding-bottom: 1rem;
	text-align: center;
	font-weight: 700;
	--tw-text-opacity: 1;
	color: rgb(54 156 140/var(--tw-text-opacity));
	text-transform: uppercase;
}

td {

	margin-top: 1.5rem;
	margin-bottom: 1.5rem;
	padding-top: 1rem;
	padding-bottom: 1rem;
	font-weight: 500;
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

blockquote,
dd,
dl,
figure,
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

.container {
	width: 100%
}

.wrap {
    text-wrap: wrap;
    word-break: normal;
    overflow-wrap: break-word;
}

article {
    border: solid;
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

.link {
	--tw-text-opacity: 1;
	color: rgb(54 156 140/var(--tw-text-opacity));
	text-decoration: underline;
	font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace;
	margin-top: .5rem;
	margin-bottom: .5rem
	font-size: 1rem;
	line-height: 1.5rem
}

.link-red {
	--tw-text-opacity: 1;
	color: rgb(255 78 51/var(--tw-text-opacity));
	text-decoration: underline;
	font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace;
	margin-top: .5rem;
	margin-bottom: .5rem
	font-size: 1rem;
	line-height: 1.5rem
}

.col-span-1 {
	grid-column: span 1/span 1
}

.col-span-3 {
	grid-column: span 3/span 3
}

.m-auto {
	margin: auto
}

.mx-auto {
	margin-left: auto;
	margin-right: auto
}

.my-2 {
	margin-top: .5rem;
	margin-bottom: .5rem
}

.my-4 {
	margin-top: 1rem;
	margin-bottom: 1rem
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

.grid {
	display: grid
}

.hidden {
	display: none
}

.w-full {
	width: 100%
}

.min-w-full {
	min-width: 100%
}

.flex-none {
	flex: none
}

.border-collapse {
	border-collapse: collapse
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

.grid-cols-2 {
	grid-template-columns: repeat(2, minmax(0, 1fr))
}

.grid-cols-3 {
	grid-template-columns: repeat(3, minmax(0, 1fr))
}

.flex-col {
	flex-direction: column
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

.justify-center {
	justify-content: center
}

.justify-between {
	justify-content: space-between
}

.justify-items-center {
	justify-items: center
}

.gap-4 {
	gap: 1rem
}

.gap-8 {
	gap: 2rem
}


.space-x-2>:not([hidden])~:not([hidden]) {
	--tw-space-x-reverse: 0;
	margin-right: calc(.5rem*var(--tw-space-x-reverse));
	margin-left: calc(.5rem*(1 - var(--tw-space-x-reverse)))
}

.space-x-4>:not([hidden])~:not([hidden]) {
	--tw-space-x-reverse: 0;
	margin-right: calc(1rem*var(--tw-space-x-reverse));
	margin-left: calc(1rem*(1 - var(--tw-space-x-reverse)))
}

.space-y-2>:not([hidden])~:not([hidden]) {
	--tw-space-y-reverse: 0;
	margin-top: calc(.5rem*(1 - var(--tw-space-y-reverse)));
	margin-bottom: calc(.5rem*var(--tw-space-y-reverse))
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

.divide-gray-600>:not([hidden])~:not([hidden]) {
	--tw-divide-opacity: 1;
	border-color: rgba(75, 85, 99, var(--tw-divide-opacity));
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

.border {
	border-width: 1px
}

.border-t-1 {
	border-top-width: 1px
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

.bg-primary-500 {
	--tw-bg-opacity: 1;
	background-color: rgb(54 156 140/var(--tw-bg-opacity))
}

.bg-primary-800 {
	--tw-bg-opacity: 1;
	background-color: rgb(32 79 74/var(--tw-bg-opacity))
}

.bg-secondary-500 {
	--tw-bg-opacity: 1;
	background-color: rgb(255 78 51/var(--tw-bg-opacity))
}

.bg-secondary-700 {
	--tw-bg-opacity: 1;
	background-color: rgb(200 38 13/var(--tw-bg-opacity))
}

.p-10 {
	padding: 2.5rem
}

.p-2 {
	padding: .5rem
}

.p-4 {
	padding: 1rem
}

.px-2 {
	padding-left: .5rem;
	padding-right: .5rem
}

.px-4 {
	padding-left: 1rem;
	padding-right: 1rem
}

.py-2 {
	padding-top: .5rem;
	padding-bottom: .5rem
}

.py-4 {
	padding-top: 1rem;
	padding-bottom: 1rem
}

.text-center {
	text-align: center
}

.text-right {
	text-align: right
}

.text-2xl {
	font-size: 1.5rem;
	line-height: 2rem
}

.text-4xl {
	font-size: 2.25rem;
	line-height: 2.5rem
}

.text-7xl {
	font-size: 4.5rem;
	line-height: 1
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

.font-bold {
	font-weight: 700
}

.font-semibold {
	font-weight: 600
}

.uppercase {
	text-transform: uppercase
}

.text-gray-300 {
	--tw-text-opacity: 1;
	color: rgb(209 213 219/var(--tw-text-opacity))
}

.text-gray-50 {
	--tw-text-opacity: 1;
	color: rgb(249 250 251/var(--tw-text-opacity))
}

.text-gray-800 {
	--tw-text-opacity: 1;
	color: rgb(31 41 55/var(--tw-text-opacity))
}

.text-primary-500 {
	--tw-text-opacity: 1;
	color: rgb(54 156 140/var(--tw-text-opacity))
}

.text-primary-800 {
	--tw-text-opacity: 1;
	color: rgb(32 79 74/var(--tw-text-opacity))
}


.text-secondary-500 {
	--tw-text-opacity: 1;
	color: rgb(255 78 51/var(--tw-text-opacity))
}

.text-secondary-700 {
	--tw-text-opacity: 1;
	color: rgb(200 38 13/var(--tw-text-opacity))
}

.shadow-md {
	--tw-shadow: 0 4px 6px -1px #0000001a, 0 2px 4px -2px #0000001a;
	--tw-shadow-colored: 0 4px 6px -1px var(--tw-shadow-color), 0 2px 4px -2px var(--tw-shadow-color)
}

.outline-0 {
	outline-width: 0
}

.ring-primary-200 {
	--tw-ring-opacity: 1;
	--tw-ring-color: rgb(171 228 214/var(--tw-ring-opacity))
}

.hover\:border-primary-200:hover {
	--tw-border-opacity: 1;
	border-color: rgb(171 228 214/var(--tw-border-opacity))
}

.hover\:border-transparent:hover {
	border-color: #0000
}

.hover\:bg-gray-200:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(229 231 235/var(--tw-bg-opacity))
}

.hover\:bg-primary-100:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(213 242 235/var(--tw-bg-opacity))
}

.hover\:bg-primary-700:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(35 98 90/var(--tw-bg-opacity))
}

.hover\:bg-primary-800:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(32 79 74/var(--tw-bg-opacity))
}

.hover\:bg-secondary-100:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(255 227 223/var(--tw-bg-opacity))
}

.hover\:bg-secondary-700:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(200 38 13/var(--tw-bg-opacity))
}

.hover\:bg-secondary-800:hover {
	--tw-bg-opacity: 1;
	background-color: rgb(165 35 15/var(--tw-bg-opacity))
}

.hover\:bg-opacity-50:hover {
	--tw-bg-opacity: 0.5
}

.hover\:font-bold:hover {
	font-weight: 700
}

.hover\:text-gray-50:hover {
	--tw-text-opacity: 1;
	color: rgb(249 250 251/var(--tw-text-opacity))
}

.hover\:text-primary-400:hover {
	--tw-text-opacity: 1;
	color: rgb(78 179 161/var(--tw-text-opacity))
}

.hover\:text-primary-50:hover {
	--tw-text-opacity: 1;
	color: rgb(243 250 249/var(--tw-text-opacity))
}

.hover\:text-primary-500:hover {
	--tw-text-opacity: 1;
	color: rgb(54 156 140/var(--tw-text-opacity))
}

.hover\:text-primary-700:hover {
	--tw-text-opacity: 1;
	color: rgb(35 98 90/var(--tw-text-opacity))
}

.hover\:text-primary-800:hover {
	--tw-text-opacity: 1;
	color: rgb(32 79 74/var(--tw-text-opacity))
}

.hover\:text-secondary-800:hover {
	--tw-text-opacity: 1;
	color: rgb(165 35 15/var(--tw-text-opacity))
}

@media (prefers-reduced-motion:reduce) {
	.motion-reduce\:transition-opacity {
		transition-property: opacity;
		transition-timing-function: cubic-bezier(.4, 0, .2, 1);
		transition-duration: .15s
	}
}

@media (min-width:768px) {
	.md\:block {
		display: block
	}

	.md\:flex {
		display: flex
	}

    .md\:grid {
		display: grid
	}

	.md\:grid-cols-3 {
		grid-template-columns: repeat(3, minmax(0, 1fr))
	}

    .md\:grid-cols-4 {
		grid-template-columns: repeat(4, minmax(0, 1fr))
	}

	.md\:grid-cols-5 {
		grid-template-columns: repeat(5, minmax(0, 1fr))
	}

    .md\:col-span-1 {
        grid-column: span 1/span 1
    }

    .md\:col-span-3 {
        grid-column: span 3/span 3
    }

	.md\:text-7xl {
		font-size: 4.5rem;
		line-height: 1
	}
}

@media (prefers-color-scheme:dark) {
	.dark\:border-gray-700 {
		--tw-border-opacity: 1;
		border-color: rgb(55 65 81/var(--tw-border-opacity))
	}

	.dark\:bg-gray-900 {
		--tw-bg-opacity: 1;
		background-color: rgb(17 24 39/var(--tw-bg-opacity))
	}

	.dark\:text-gray-300 {
		--tw-text-opacity: 1;
		color: rgb(209 213 219/var(--tw-text-opacity))
	}

	.dark\:after\:bg-gray-300:after {
		content: var(--tw-content);
		--tw-bg-opacity: 1;
		background-color: rgb(209 213 219/var(--tw-bg-opacity))
	}

	.peer:checked~.dark\:peer-checked\:bg-primary-500 {
		--tw-bg-opacity: 1;
		background-color: rgb(54 156 140/var(--tw-bg-opacity))
	}

	.peer:focus~.dark\:peer-focus\:outline-gray-300 {
		outline-color: #d1d5db
	}

	.peer:focus:checked~.dark\:peer-focus\:peer-checked\:outline-primary-500 {
		outline-color: #369c8c
	}
}

|}

const GRID_SIZE = 32
const WORKGROUP_SIZE = 8

const canvas0 = document.querySelector("#c0")
const logem = document.getElementById('loginfo')
const startBtn = document.getElementById('startBtn')
const toggleBtn = document.getElementById('toggleBtn')
const stepBtn = document.getElementById('stepBtn')
const rateSlider = document.getElementById('rate')
const rateValue = document.getElementById('rateValue')
const intervalSlider = document.getElementById('interval')
const intervalValue = document.getElementById('intervalValue')

let step = 0
let update_interval = 100
let rate = 0.6
let initGridFunc = null
let exefn = null

let timerHandle = null

const yylog = (str) => {
    logem.textContent = str
}

startBtn.addEventListener('click', () => {
    if(!timerHandle) {
	initGridFunc()
	timerHandle = setInterval(exefn, update_interval)
	toggleBtn.textContent = 'Pause'
	yylog('pause -> running, Press "Start" to rerun or "Pause" to pause')
	stepBtn.disabled = true
    } else {
	clearInterval(timerHandle)
	initGridFunc()
	timerHandle = setInterval(exefn, update_interval)
	yylog('running -> rerunning, Press "Start" to rerun or "Pause" to pause')
	toggleBtn.textContent = 'Pause'
	stepBtn.disabled = true
    }
})

toggleBtn.addEventListener('click', () => {
    if(!timerHandle) {
	timerHandle = setInterval(exefn, update_interval)
	toggleBtn.textContent = "Pause"
	yylog('pause -> running, Press "Start" to rerun or "Pause" to pause')
	stepBtn.disabled = true
    } else {
	clearInterval(timerHandle)
	timerHandle = null
	toggleBtn.textContent = "Run"
	yylog('running -> pause, Press "Start" to rerun or "Run" to continue run')
	stepBtn.disabled = false
    }
})

stepBtn.addEventListener('click', () => {
    exefn()
})

rateSlider.oninput = function() {
    rate = this.value / 100.0
    rateValue.textContent = this.value + '%'
}

intervalSlider.oninput = function() {
    update_interval = this.value
    intervalValue.textContent = this.value + 'ms'
}

async function init() {
    let screenWidth = window.innerWidth
    if (screenWidth < 512) {
	canvas0.weight = screenWidth
	canvas0.height = screenWidth
    }
    if (!navigator.gpu) {
	yylog('WebGPU not supported!')
	canvas0.remove()
	return false
    }
    const adapter = await navigator.gpu.requestAdapter()
    if (!adapter) {
	yylog('no GPUAdapter found!')
	canvas0.remove()
	return false
    }
    const device = await adapter.requestDevice()

    device.lost.then((info) => {
	console.log("webgpu was lost")
	device = null
	if (info.reason != 'destoryed') {
	    init()
	}
    })
    //canvas.getContext('2d').fillRect(0, 0, canvas.width, canvas.height)
    const context = canvas0.getContext("webgpu")
    const canvasFormat = navigator.gpu.getPreferredCanvasFormat()

    context.configure({
	device: device,
	format: canvasFormat,
    })

    const uniformArray = new Float32Array([GRID_SIZE, GRID_SIZE])
    const uniformBuffer = device.createBuffer({
	label: "Grid Uniforms",
	size: uniformArray.byteLength,
	usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    })
    device.queue.writeBuffer(uniformBuffer, 0, uniformArray)

    const cellStateArray = new Uint32Array(GRID_SIZE * GRID_SIZE)
    const cellStateStorage = [
	device.createBuffer({
	    label: "Cell State",
	    size: cellStateArray.byteLength,
	    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
	}),
	device.createBuffer({
	    label: "Cell State",
	    size: cellStateArray.byteLength,
	    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
	})
    ]

    initGridFunc = () => {
	for (let i = 0; i < cellStateArray.length; i += 3) {
	    cellStateArray[i] = Math.random() < rate ? 1 : 0
	}
	device.queue.writeBuffer(cellStateStorage[0], 0, cellStateArray)
	device.queue.writeBuffer(cellStateStorage[1], 0, cellStateArray)
    }
    initGridFunc()

    const vertices = new Float32Array([
	-0.8, -0.8, 0.8, -0.8, 0.8, 0.8, // triangle 1
	-0.8, -0.8, 0.8, 0.8, -0.8, 0.8, // triangle 2
    ])
    const vertexBuffer = device.createBuffer({
	label: "Cell vertices",
	size: vertices.byteLength,
	usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    })
    device.queue.writeBuffer(vertexBuffer, 0, vertices)

    const vertexBufferLayout = {
	arrayStride: 8,
	attributes: [{
	    format: 'float32x2',
	    offset: 0,
	    shaderLocation: 0,
	}]
    }

    const cellShaderModule = device.createShaderModule({
	label: 'Cell shader',
	code: `
struct VertexInput {
  @location(0) pos: vec2f,
  @builtin(instance_index) instance: u32
};
struct VertexOutput {
  @builtin(position) pos: vec4f,
  @location(0) cell: vec2f
};
@group(0) @binding(0) var<uniform> grid: vec2f;
@group(0) @binding(1) var<storage> cellState: array<u32>;
@vertex
fn vertexMain(input : VertexInput) -> VertexOutput {
    let i = f32(input.instance);
    let cell = vec2f(i % grid.x, floor(i / grid.x));
    let state = f32(cellState[input.instance]);
    let cellOffset = cell / grid * 2.0;
    let gridPos = (input.pos * state + 1.0) / grid - 1.0 + cellOffset;
    var output: VertexOutput;
    output.pos = vec4f(gridPos, 0, 1);
    output.cell = cell;
    return output;
}
@fragment
fn fragmentMain(input: VertexOutput) -> @location(0) vec4f {
    let c = input.cell / grid;
    return vec4f(c, 1-c.x, 1);
}
`
    })

    const simulationShaderModule = device.createShaderModule({
	label: "Game of Life simulation shader",
	code: `
@group(0) @binding(0) var<uniform> grid: vec2f;
@group(0) @binding(1) var<storage> cellStateIn: array<u32>;
@group(0) @binding(2) var<storage, read_write> cellStateOut: array<u32>;
fn cellIndex(cell: vec2u) -> u32 {
    return (cell.y % u32(grid.y)) * u32(grid.x) +
           (cell.x % u32(grid.x));
}
fn callActive(x: u32, y: u32) -> u32 {
    return cellStateIn[cellIndex(vec2(x, y))];
}
@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn computeMain(@builtin(global_invocation_id) cell: vec3u) {
    let activeNegihbors = callActive(cell.x + 1, cell.y + 1) +
                          callActive(cell.x + 1, cell.y) +
                          callActive(cell.x + 1, cell.y - 1) +
                          callActive(cell.x, cell.y - 1) +
                          callActive(cell.x - 1, cell.y - 1) +
                          callActive(cell.x - 1, cell.y) +
                          callActive(cell.x - 1, cell.y + 1) +
                          callActive(cell.x, cell.y + 1);
    let i = cellIndex(cell.xy);
    switch activeNegihbors {
      case 2: {
        cellStateOut[i] = cellStateIn[i];
      }
      case 3: {
        cellStateOut[i] = 1;
      }
      default: {
        cellStateOut[i] = 0;
      }
    }
}
`
    })

    const bindGroupLayout = device.createBindGroupLayout({
	label: "Cell Bind Group Layout",
	entries: [{
	    binding: 0,
	    visibility: GPUShaderStage.VERTEX
		| GPUShaderStage.COMPUTE
		| GPUShaderStage.FRAGMENT,
	    buffer: {}
	}, {
	    binding: 1,
	    visibility: GPUShaderStage.VERTEX | GPUShaderStage.COMPUTE,
	    buffer: {type: "read-only-storage"}
	}, {
	    binding: 2,
	    visibility: GPUShaderStage.COMPUTE,
	    buffer: {type: "storage"}
	}]
    })

    const bindGroups = [
	device.createBindGroup({
	    label: "Cell renderer bind group A",
	    layout: bindGroupLayout,
	    entries: [{
		    binding: 0,
		    resource: {buffer: uniformBuffer}
		}, {
		    binding: 1,
		    resource: {buffer: cellStateStorage[0]}
		}, {
		    binding: 2,
		    resource: {buffer: cellStateStorage[1]}
		}]
	}),
	device.createBindGroup({
	    label: "Cell renderer bind group B",
	    layout: bindGroupLayout,
	    entries: [{
		    binding: 0,
		    resource: {buffer: uniformBuffer}
		}, {
		    binding: 1,
		    resource: {buffer: cellStateStorage[1]}
		}, {
		    binding: 2,
		    resource: {buffer: cellStateStorage[0]}
		}]
	})
    ]

    const pipelineLayout = device.createPipelineLayout({
	label: "Cell pipeline Layout",
	bindGroupLayouts: [ bindGroupLayout ]
    })

    const cellPipeline = device.createRenderPipeline({
	label: "Cell pipeline",
	layout: pipelineLayout,
	vertex: {
	    module: cellShaderModule,
	    entryPoint: "vertexMain",
	    buffers: [vertexBufferLayout]
	},
	fragment: {
	    module: cellShaderModule,
	    entryPoint: "fragmentMain",
	    targets: [{
		format: canvasFormat
	    }]
	}
    })

    const simulationPipeline = device.createComputePipeline({
	label: "Simulation pipeline",
	layout: pipelineLayout,
	compute: {
	    module: simulationShaderModule,
	    entryPoint: "computeMain",
	}
    })

    step = 0
    const encoder = device.createCommandEncoder()
    const pass = encoder.beginRenderPass({
	    colorAttachments: [{
		view: context.getCurrentTexture().createView(),
		loadOp: "clear",
		clearValue: [0, 0, 0, 1.0],
		storeOp: "store",
	    }]
    })
    pass.end()
    device.queue.submit([encoder.finish()])

    exefn = () => {
	const encoder = device.createCommandEncoder()

	const computePass = encoder.beginComputePass()
	computePass.setPipeline(simulationPipeline)
	computePass.setBindGroup(0, bindGroups[step % 2])

	const workgroupCount = Math.ceil(GRID_SIZE / WORKGROUP_SIZE)
	computePass.dispatchWorkgroups(workgroupCount, workgroupCount)

	computePass.end()

	step++

	const pass = encoder.beginRenderPass({
	    colorAttachments: [{
		view: context.getCurrentTexture().createView(),
		loadOp: "clear",
		clearValue: [0, 0, 0, 1.0],
		storeOp: "store",
	    }]
	})

	pass.setPipeline(cellPipeline)
	pass.setBindGroup(0, bindGroups[step%2])
	pass.setVertexBuffer(0, vertexBuffer)
	pass.draw(vertices.length / 2, GRID_SIZE * GRID_SIZE)

	pass.end()

	device.queue.submit([encoder.finish()])
    }
    startBtn.disabled = false
    toggleBtn.disabled = false
    stepBtn.disabled = false
    yylog('ready to run, press "Start" or "Run" to fire it!')
}

init()


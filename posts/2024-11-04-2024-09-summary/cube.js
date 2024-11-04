const mat4 = wgpuMatrix.mat4
const vec3 = wgpuMatrix.vec3


const cubeVertexSize = 4 * 10;
const cubePositionOffset = 0;
const cubeColorOffset = 4 * 4;
const cubeUVOffset = 4 * 8;
const cubeVertexCount = 36;

const cubeVertexArray = new Float32Array([
    1, -1, 1, 1,   1, 0, 1, 1,  0, 1,
    -1, -1, 1, 1,  0, 0, 1, 1,  1, 1,
    -1, -1, -1, 1, 0, 0, 0, 1,  1, 0,
    1, -1, -1, 1,  1, 0, 0, 1,  0, 0,
    1, -1, 1, 1,   1, 0, 1, 1,  0, 1,
    -1, -1, -1, 1, 0, 0, 0, 1,  1, 0,

    1, 1, 1, 1,    1, 1, 1, 1,  0, 1,
    1, -1, 1, 1,   1, 0, 1, 1,  1, 1,
    1, -1, -1, 1,  1, 0, 0, 1,  1, 0,
    1, 1, -1, 1,   1, 1, 0, 1,  0, 0,
    1, 1, 1, 1,    1, 1, 1, 1,  0, 1,
    1, -1, -1, 1,  1, 0, 0, 1,  1, 0,

    -1, 1, 1, 1,   0, 1, 1, 1,  0, 1,
    1, 1, 1, 1,    1, 1, 1, 1,  1, 1,
    1, 1, -1, 1,   1, 1, 0, 1,  1, 0,
    -1, 1, -1, 1,  0, 1, 0, 1,  0, 0,
    -1, 1, 1, 1,   0, 1, 1, 1,  0, 1,
    1, 1, -1, 1,   1, 1, 0, 1,  1, 0,

    -1, -1, 1, 1,  0, 0, 1, 1,  0, 1,
    -1, 1, 1, 1,   0, 1, 1, 1,  1, 1,
    -1, 1, -1, 1,  0, 1, 0, 1,  1, 0,
    -1, -1, -1, 1, 0, 0, 0, 1,  0, 0,
    -1, -1, 1, 1,  0, 0, 1, 1,  0, 1,
    -1, 1, -1, 1,  0, 1, 0, 1,  1, 0,

    1, 1, 1, 1,    1, 1, 1, 1,  0, 1,
    -1, 1, 1, 1,   0, 1, 1, 1,  1, 1,
    -1, -1, 1, 1,  0, 0, 1, 1,  1, 0,
    -1, -1, 1, 1,  0, 0, 1, 1,  1, 0,
    1, -1, 1, 1,   1, 0, 1, 1,  0, 0,
    1, 1, 1, 1,    1, 1, 1, 1,  0, 1,

    1, -1, -1, 1,  1, 0, 0, 1,  0, 1,
    -1, -1, -1, 1, 0, 0, 0, 1,  1, 1,
    -1, 1, -1, 1,  0, 1, 0, 1,  1, 0,
    1, 1, -1, 1,   1, 1, 0, 1,  0, 0,
    1, -1, -1, 1,  1, 0, 0, 1,  0, 1,
    -1, 1, -1, 1,  0, 1, 0, 1,  1, 0,
]);


const canvas = document.querySelector('#c1')
const sampleCount = 4
let device

async function getCode(fname) {
    let res = await fetch(fname)
    let code = await res.text()
    return code
}

async function init(f) {
    const adapter = await navigator.gpu.requestAdapter()
    device = await adapter.requestDevice()
    await f()
}

init(async () => {
    const context = canvas.getContext('webgpu')
    const preFormat = navigator.gpu.getPreferredCanvasFormat()
    context.configure({
	device,
	format: preFormat,
	alphaMode: 'premultiplied',
    })

    const verticesBuffer = device.createBuffer({
	size: cubeVertexArray.byteLength,
	usage: GPUBufferUsage.VERTEX,
	mappedAtCreation: true,
    })
    new Float32Array(verticesBuffer.getMappedRange()).set(cubeVertexArray)
    verticesBuffer.unmap()

    const pipeline = device.createRenderPipeline({
	layout: 'auto',
	vertex: {
	    module: device.createShaderModule({
		code: `
struct Uniforms {
  modelViewProjectionMatrix : mat4x4f,
}
@binding(0) @group(0) var<uniform> uniforms : Uniforms;

struct VertexOutput {
  @builtin(position) Position : vec4f,
  @location(0) fragUV : vec2f,
  @location(1) fragPosition: vec4f,
}

@vertex
fn main(
  @location(0) position : vec4f,
  @location(1) uv : vec2f
) -> VertexOutput {
  var output : VertexOutput;
  output.Position = uniforms.modelViewProjectionMatrix * position;
  output.fragUV = uv;
  output.fragPosition = 0.5 * (position + vec4(1.0, 1.0, 1.0, 1.0));
  return output;
}
`,
	    }),
	    buffers: [
		{
		    arrayStride: cubeVertexSize,
		    attributes: [
			{
			    shaderLocation: 0,
			    offset: cubePositionOffset,
			    format: 'float32x4',
			},
			{
			    shaderLocation: 1,
			    offset: cubeUVOffset,
			    format: 'float32x2',
			}
		    ]
		}
	    ]
	},
	fragment: {
	    module: device.createShaderModule({
		code: `
@fragment
fn main(
  @location(0) fragUV: vec2f,
  @location(1) fragPosition: vec4f
) -> @location(0) vec4f {
  return fragPosition;
}
`,
	    }),
	    targets: [
		{format: preFormat},
	    ],
	},
	primitive: {
	    topology: 'triangle-list',
	    cullMode: 'back',
	},
	depthStencil: {
	    depthWriteEnabled: true,
	    depthCompare: 'less',
	    format: 'depth24plus',
	}
    })
    const depthTexture = device.createTexture({
	size: [480, 480],
	format: 'depth24plus',
	usage: GPUTextureUsage.RENDER_ATTACHMENT,
    })
    const uniformBufferSize = 4 * 16
    const uniformBuffer = device.createBuffer({
	size: uniformBufferSize,
	usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    })

    const uniformBindGroup = device.createBindGroup({
	layout: pipeline.getBindGroupLayout(0),
	entries: [
	    {
		binding: 0,
		resource: {
		    buffer: uniformBuffer,
		}
	    }
	]
    })

    const renderPassDescriptor = {
	colorAttachments: [
	    {
		view: undefined,

		clearValue: [0.5,0.5,0.5,1],
		loadOp: 'clear',
		storeOp: 'store',
	    }
	],
	depthStencilAttachment: {
	    view: depthTexture.createView(),
	    depthClearValue: 1.0,
	    depthLoadOp: 'clear',
	    depthStoreOp: 'store',
	}
    }
    const aspect = 1.0
    const projectionMatrix = mat4.perspective((2 * Math.PI) / 5.0, aspect, 1.0, 100.0)
    const modelViewPorjectionMatrix = mat4.create()

    function getTransformationMatrix() {
	const viewMatrix = mat4.identity()
	mat4.translate(viewMatrix, vec3.fromValues(0, 0, -4), viewMatrix)
	const now = Date.now() / 1000
	mat4.rotate(
	    viewMatrix,
	    vec3.fromValues(Math.sin(now), Math.cos(now), 0),
	    1,
	    viewMatrix
	)
	mat4.multiply(projectionMatrix, viewMatrix, modelViewPorjectionMatrix)
	return modelViewPorjectionMatrix
    }

    function frame() {
	const transformationMatrix = getTransformationMatrix();
	device.queue.writeBuffer(
	    uniformBuffer,
	    0,
	    transformationMatrix.buffer,
	    transformationMatrix.byteoffset,
	    transformationMatrix.byteLength,
	)
	renderPassDescriptor.colorAttachments[0].view = context
	    .getCurrentTexture()
	    .createView()
	const encoder = device.createCommandEncoder()
	const pass = encoder.beginRenderPass(renderPassDescriptor)
	pass.setPipeline(pipeline)
	pass.setBindGroup(0, uniformBindGroup)
	pass.setVertexBuffer(0, verticesBuffer)
	pass.draw(cubeVertexCount)
	pass.end()
	device.queue.submit([encoder.finish()])
	requestAnimationFrame(frame)
    }

    requestAnimationFrame(frame)

})
